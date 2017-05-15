#lang scribble/acmart @acmlarge
@(require scriblib/figure 
          scribble/manual 
          scriblib/footnote
          scribble/eval
          racket/match
          "evals.rkt"
          "bib.rkt")

@title[#:tag "s:interp"]{A Definitional Interpreter}

@;{
\begin{wrapfigure}{R}{0.5\textwidth} %{{{ f:syntax
  \begin{mdframed}
    \begin{alignat*}{4}
      e ∈ &&\mathrel{}   exp ⩴ &\mathrel{} 𝔥⸨(vbl⸩\ x𝔥⸨)⸩         &\hspace{3em} [⦑\emph{variable}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(num⸩\ n𝔥⸨)⸩         &\hspace{3em} [⦑\emph{number}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(if0⸩\ e\ e\ e𝔥⸨)⸩   &\hspace{3em} [⦑\emph{conditional}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(op2⸩\ b\ e\ e𝔥⸨)⸩   &\hspace{3em} [⦑\emph{binary op}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(app⸩\ e\ e𝔥⸨)⸩      &\hspace{3em} [⦑\emph{application}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(rec⸩\ x\ ℓ\ e𝔥⸨)⸩   &\hspace{3em} [⦑\emph{letrec}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} ℓ                     &\hspace{3em} [⦑\emph{lambda}⦒]
      \\[\mathgobble]ℓ ∈ &&\mathrel{}   lam ⩴ &\mathrel{} 𝔥⸨(lam⸩\ x\ e𝔥⸨)⸩      &\hspace{3em} [⦑\emph{function defn}⦒]
      \\[\mathgobble] x ∈ &&\mathrel{}   var ≔ &\mathrel{} ❴𝔥⸨x⸩, 𝔥⸨y⸩, …❵        &\hspace{3em} [⦑\emph{variable names}⦒]
      %\\[\mathgobble] u ∈ &&\mathrel{}  unop ≔ &\mathrel{} ❴𝔥⸨add1⸩, …❵           &\hspace{3em} [⦑\emph{unary prim}⦒]
      \\[\mathgobble] b ∈ &&\mathrel{} binop ≔ &\mathrel{} ❴𝔥⸨+⸩, 𝔥⸨-⸩, …❵        &\hspace{3em} [⦑\emph{binary prim}⦒]
    \end{alignat*}
    \captionskip{Programming Language Syntax}
    \label{f:syntax}
  \end{mdframed}
\end{wrapfigure} %}}}
}

@figure["f:syntax" "Programming Language Syntax" "FIXME"]

We begin by constructing a definitional interpreter for a small but
representative higher-order, functional language.  The abstract syntax
of the language is defined in @Figure-ref{f:syntax}; it includes
variables, numbers, binary operations on numbers, conditionals,
@racket[letrec] expressions, functions and applications.

The interpreter for the language is defined in
@Figure-ref{f:interpreter}. At first glance, it has many conventional
aspects: it is compositionally defined by structural recursion on the
syntax of expressions; it represents function values as a closure data
structure which pairs the lambda term with the evaluation environment;
it is structured monadically and uses monad operations to interact
with the environment and store; and it relies on a helper function
@racket[δ] to interpret primitive operations.

There are a few superficial aspects that deserve a quick note:
environments @racket[ρ] are finite maps and the syntax @racket[(ρ x)]
denotes @math{ρ(x)} while @racket[(ρ x a)] denotes @math{ρ[x↦a]}.  For
simplicity, recursive function definitions (@racket[rec]) are assumed
to be syntactic values.  The @racket[do]-notation is just shorthand for
@racket[bind], as usual:
@racketblock[
  (do x ← e . r) ≡ (bind e (λ (x) (do . r)))
       (do e . r) ≡ (bind e (λ (_) (do . r)))
   (do x ≔ e . r) ≡ (let ((x e)) (do . r))
           (do b) ≡ b
]
Finally, there are two unconventional aspects worth noting.

First, the interpreter is written in an @emph{open recursive style};
the evaluator does not call itself recursively, instead it takes as an
argument a function @racket[ev]—shadowing the name of the function
@racket[ev] being defined—and @racket[ev] (the argument) is called
instead of self-recursion.  This is a standard encoding for recursive
functions in a setting without recursive binding.  It is up to an
external function, such as the Y-combinator, to close the recursive
loop.  This open recursive form is crucial because it allows
intercepting recursive calls to perform “deep” instrumentation of the
interpreter.

Second, the code is clearly @emph{incomplete}.  There are a number of free
variables, typeset as italics, which implement the following:

@itemlist[
@item{The underlying monad of the interpreter: @racket[_return] and @racket[_bind];}
@item{An interpretation of primitives: @racket[_δ] and @racket[_zero?];}
@item{Environment operations: @racket[_ask-env] for retrieving the
environment and @racket[_local-env] for installing an environment;}
@item{Store operations: @racket[_ext] for updating the store, and @racket[_find] for
dereferencing locations; and}
@item{An operation for @racket[_alloc]ating new store locations.}
]

Going forward, we make frequent use of definitions involving free
variables, and we call such a collection of such definitions a
@emph{component}. We assume components can be named (in this case,
we've named the component @racket[ev@], indicated by the box in the
upper-right corner) and linked together to eliminate free
variables.@note{We use Racket @emph{units} @~cite{local:flatt-pldi98}
to model components in our implementation.}

@figure["f:interpreter" "The Extensible Definitional Interpreter"]{
@filebox[@racket[ev@]]{
@racketblock[
(define ((ev ev) e)
  (match e
    [(num n)
     (_return n)]
    [(vbl x)
     (do ρ ← _ask-env
         (_find (ρ x)))]    
    [(if0 e₀ e₁ e₂)
     (do v  ← (ev e₀)  z? ← (_zero? v)
         (ev (if z? e₁ e₂)))]
    [(op2 o e₀ e₁)
     (do v₀ ← (ev e₀)  v₁ ← (ev e₁)
         (_δ o v₀ v₁))]
    [(rec f l e)
     (do ρ  ← ask-env  a  ← (_alloc f)
         ρ′ ≔ (ρ f a)
         (_ext a (cons l ρ′))
         (_local-env ρ′ (ev e)))]
    [(lam x e₀)
     (do ρ ← _ask-env
         (_return (cons (lam x e₀) ρ)))]
    [(app e₀ e₁)
     (do (cons (lam x e₂) ρ) ← (ev e₀)
          v₁ ← (ev e₁)
          a  ← (_alloc x)
          (_ext a v₁)
          (_local-env (ρ x a) (ev e₂)))]))
]}}

Next we examine a set of components which complete the definitional
interpreter, shown in @Figure-ref{f:concrete-components}. The first
component @racket[monad@] uses a macro @racket[define-monad] which
generates a set of bindings based on a monad transformer stack.  We
use a failure monad to model divide-by-zero errors, a state monad to
model the store, and a reader monad to model the environment.  The
@racket[define-monad] form generates bindings for @racket[return],
@racket[bind], @racket[ask-env], @racket[local-env],
@racket[get-store] and @racket[update-store]; their definitions are
standard @~cite{dvanhorn:Liang1995Monad}.

We also define @racket[run] for running monadic computations, starting with the empty
environment and store @racket[∅]:
@racketblock[
(define (mrun m)
  (run-StateT ∅ (run-ReaderT ∅ m)))
]
While the @racket[define-monad] form is hiding some details, this
component could have equivalently been written out explicitly. For
example, @racket[return] and @racket[bind] can be defined as:
@racketblock[
(define (((return a) r) s) (cons a s))
(define (((bind ma f) r) s)
  (match ((ma r) s)
    [(cons a s′) (((f a) r) s′)]
    ['failure 'failure]))
]
So far our use of monad transformers is as a mere convenience, however
the monad abstraction will become essential for easily deriving new
analyses later on.

@figure["f:concrete-components" "Components for Definitional Interpreters"]{
@filebox[@racket[monad@]]{
@racketblock[
(define-monad (ReaderT (FailT (StateT ID))))]}
@filebox[@racket[δ@]]{
@racketblock[
(define (δ o n₀ n₁)
  (match o
    ['+ (return (+ n₀ n₁))]
    ['- (return (- n₀ n₁))]
    ['* (return (* n₀ n₁))]
    ['/ (if (= 0 n₁) fail (return (/ n₀ n₁)))]))
(define (zero? v) (return (= 0 v)))]}
@filebox[@racket[store@]]{
@racketblock[
(define (find a)  (do σ ← get-store
                      (return (σ a))))
(define (ext a v) (update-store (λ (σ) (σ a v))))]}
@filebox[@racket[alloc@]]{
@racketblock[
(define (alloc x) (do σ ← get-store
                      (return (size σ))))]}
}

The @racket[δ@] component defines the interpretation of primitives,
which is given in terms of the underlying monad.  The @racket[alloc@]
component provides a definition of @racket[alloc], which fetches the
store and uses its size to return a fresh address, assuming the
invariant @racket[(∈ a σ)] @math{⇔} @racket[(< a (size σ))].  The
@racket[alloc] function takes a single argument, which is the name of
the variable whose binding is being allocated.  For the time being, it
is ignored, but will become relevant when abstracting closures
(@secref{s:abstracting-closures}).  The @racket[store@] component
defines @racket[find] and @racket[ext] for finding and extending
values in the store.

@figure["f:trace" "Trace Collecting Semantics"]{
@filebox[@racket[trace-monad@]]{
@racketblock[
(define-monad (ReaderT (FailT (StateT (WriterT List ID)))))
]}
@filebox[@racket[ev-tell@]]{
@racketblock[
(define (((ev-tell ev₀) ev) e)
  (do ρ ← ask-env  σ ← get-store
      (tell (list e ρ σ))
      ((ev₀ ev) e)))
]}}

The only remaining pieces of the puzzle are a fixed-point combinator and the
main entry-point for the interpreter, which are straightforward to define:
@racketblock[
(define ((fix f) x) ((f (fix f)) x))
(define (eval e) (mrun ((fix ev) e)))
]

By taking advantage of Racket's languages-as-libraries features
@~cite{dvanhorn:TobinHochstadt2011Languages}, we construct REPLs for
interacting with this interpreter.  Here are a few evaluation examples
in a succinct concrete syntax:

@interaction[#:eval the-pure-eval
@code:comment{Closure over the empty environment paired with the empty store.}
(λ (x) x)

@code:comment{Closure over a non-empty environment and store.}
((λ (x) (λ (y) x)) 4)

@code:comment{Primitive operations work as expected.}
(* (+ 3 4) 9)

@code:comment{Divide-by-zero errors result in failures.}
(quotient 5 (- 3 3))   
]
Because our monad stack places @racket[FailT] above @racket[StateT],
the answer includes the (empty) store at the point of the error. Had
we changed @racket[monad@] to use @racket[(ReaderT (StateT (FailT
ID)))] then failures would not include the store:
@interaction[#:eval the-pure-eval-alt
(quotient 5 (- 3 3))   
]
At this point we've defined a simple definitional interpreter, although the
extensible components involved—monadic operations and open recursion—will allow
us to instantiate the same interpreter to achieve a wide range of useful
abstract interpretations.

@section[#:tag "s:collecting"]{Collecting Variations}

The formal development of abstract interpretation often starts from a
so-called ``non-standard collecting semantics.''  A common form of
collecting semantics is a trace semantics, which collects streams of
states the interpreter reaches.  @Figure-ref{f:trace} shows the monad
stack for a tracing interpreter and a ``mix-in'' for the evaluator.
The monad stack adds @racket[WriterT List], which provides a new
operation @racket[tell] for writing lists of items to the stream of
reached states.  The @racket[ev-tell] function is a wrapper around an
underlying @racket[ev₀] unfixed evaluator, and interposes itself
between each recursive call by @racket[tell]ing the current state of
the evaluator: the current expression, environment and store.  The
top-level evaluation function is then: 
@racketblock[
(define (eval e) (mrun ((fix (ev-tell ev)) e)))
]

@figure["f:dead" "Dead Code Collecting Semantics"]{
@filebox[@racket[dead-monad@]]{
@racketblock[
(define-monad
  (ReaderT (StateT (StateT (FailT ID)))))]}
@filebox[@racket[ev-dead@]]{
@racketblock[
(define (((ev-dead ev₀) ev) e)
  (do θ ← get-dead       
      (put-dead (set-remove θ e))
      ((ev₀ ev) e)))]}
@filebox[@racket[eval-dead@]]{
@racketblock[
(define ((eval-dead eval) e₀)
  (do (put-dead (subexps e₀))
      (eval e₀)))]}
}


Now when an expression is evaluated, we get an answer and a list of all states
seen by the evaluator, in the order in which they were seen. For example:
@;(not showing @racket[ρ] or @racket[σ] in results):
@interaction[#:eval the-trace-eval
(* (+ 3 4) 9)]
Were we to swap @racket[List] with @racket[Set] in the monad stack, we would obtain a
@emph{reachable} state semantics, another common form of collecting semantics,
that loses the order and repetition of states.

As another collecting semantics variant, we show how to collect the
@emph{dead code} in a program.  Here we use a monad stack that has an
additional state component (with operations named @racket[put-dead] and
@racket[get-dead]) which stores the set of dead expressions.  Initially this
will contain all subexpressions of the program.  As the interpreter
evaluates expressions it will remove them from the dead set.

@Figure-ref{f:dead} defines the monad stack for the dead code
collecting semantics and the @racket[ev-dead@] component, another
mix-in for an @racket[ev₀] evaluator to remove the given subexpression
before recurring.  Since computing the dead code requires an outer
wrapper that sets the initial set of dead code to be all of the
subexpressions in the program, we define @racket[eval-dead@] which
consumes a @emph{closed evaluator}, i.e. something of the form
@racket[(fix ev)]. Putting these pieces together, the dead code
collecting semantics is defined:
@racketblock[ 
(define (eval e) (mrun ((eval-dead (fix (ev-dead ev))) e)))]

Running a program with the dead code interpreter produces an answer and the set
of expressions that were not evaluated during the running of a program:
@interaction[#:eval the-dead-eval
(if0 0 1 2)
(λ (x) x)
(if0 (quotient 1 0) 2 3)]

Our setup makes it easy not only to express the concrete interpreter,
but also these useful forms of collecting semantics.

@section[#:tag "s:base"]{Abstracting Base Values}

Our interpreter must become decidable before it can be considered an analysis,
and the first step towards decidability is to abstract the base types of the
language to something finite. We do this for our number base type by
introducing a new @emph{abstract} number, written @racket['N], which represents the
set of all numbers. Abstract numbers are introduced by an alternative
interpretation of primitive operations, given in @Figure-ref{f:abs-delta},
which simply produces @racket['N] in all cases. 

Some care must be taken in the abstraction of @racket['quotient]. If
the denominator is the abstract number @racket['N], then it is
possible the program could fail as a result of divide-by-zero, since
@racket[0] is contained in the interpretation of
@racket['N]. Therefore there are @emph{two} possible answers when the
denominator is @racket['N]: @racket['N] and @racket['failure]. Both
answers are @racket[return]ed by introducing non-determinism
@racket[NondetT] into the monad stack. Adding non-determinism provides
the @racket[mplus] operation for combining multiple
answers. Non-determinism is also used in @racket[zero?], which returns
both true and false on @racket['N].

By linking together @racket[δ^@] and the monad stack with
non-determinism, we obtain an evaluator that produces a set of
results:
@interaction[#:eval the-abs-delta-eval
(* (+ 3 4) 9)
(quotient 5 (+ 1 2))
(if0 (+ 1 0) 3 4)]

@;{FIXME
If we link @racket[δ^@] with the @emph{tracing} monad stack plus
non-determinism:
...
}




It is clear that the interpreter will only ever see a finite set of
numbers (including @racket['N]), but it's definitely not true that the
interpreter halts on all inputs.  First, it's still possible to
generate an infinite number of closures.  Second, there's no way for
the interpreter to detect when it sees a loop.  To make a terminating
abstract interpreter requires tackling both.  We look next at
abstracting closures.






