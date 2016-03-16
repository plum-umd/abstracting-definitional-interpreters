#lang scribble/sigplan @nocopyright
@(require "bib.rkt"
	  "evals.rkt")
@(require scribble/manual
	  scriblib/figure
	  scribble/decode
	  scriblib/footnote
	  scribble/eval
	  racket/pretty)

@title{Abstracting Definitional Interpreters @subtitle{Functional Pearl}}

@;title{Definitional Abstract Interpreters for Higher-Order Programming Languages}

@authorinfo["" "" ""]

@abstract{A definitional interpreter written in monadic style can
express a wide variety of abstract interpretations.  We give a
rational reconstruction of a definitional abstract interpreter for a
higher-order language by constructing a series of units implementing
monadic operations.  The denouement of our story is a sound and
computable abstract interpreter that arises from the composition of
simple, independent components.  Remarkably, this interpreter
implements a form of pushdown control flow analysis (PDCFA) in which
calls and returns are always properly matched in the abstract
semantics.  True to the definitional style of Reynolds, the evaluator
involves no explicit mechanics to achieve this property; it is simply
inherited from the defining language.}

@keywords{definitional interpreters, abstract interpretation, pushdown
control flow analysis, symbolic execution}

@section{Introduction}

In his landmark paper, @emph{Definitional interpreters for
higher-order languages}@~cite[reynolds72], Reynolds first observed
that when a language is defined by way of an interpreter, it is
possible for the defined language to inherit semantic characteristics
of the defining language of the interpreter.  For example, it is easy
to write a compositional evaluator that defines a call-by-value
language if the defining language is call-by-value, but defines a
call-by-name language if the defining language is call-by-name.

In this paper, we make the following two observations:

@itemlist[#:style 'ordered

@item{Definitional interpreters, written in monadic style,
can simultaneously define a language's semantics as well as
safe approximations of those semantics.}

@item{These definitional abstract interpreters can inherit
characteristics of the defining language.  In particular, we show that
the abstract interpreter inherits the call and return matching
property of the defining language and therefore realizes an abstract
intpretation in the pushown style of analyses@~cite[cfa2-diss
pdcfa-sfp10].}

]

A common problem of past approaches to the control flow analysis of
functional languages is the inability to properly match a function
call with its return in the abstract semantics, leading to infeasible
program (abstract) executions in which a return is made from one point
in the program text, but control units to another.  The CFA2
analysis of Vardoulakis and Shivers@~cite[cfa2-lmcs] was the first
approach that overcame this shortcoming.  In essence, this kind of
analysis can be viewed as replacing the traditional finite automata
abstractions of programs with pushdown automata@~cite[pdcfa-sfp10].

In this paper we investigate the use of definitional interpreters as
the basis for abstract interpretation of higher-order languages.  We
show that a definitional interpreter---a compositional evaluation
function---written in a monadic and componential style can express a
wide variety of concrete and abstract interpretations.  

@section{From Machines to Compositional Evaluators}

In recent years, there has been considerable effort in the systematic
construction of abstract interpreters for higher-order languages using
abstract machines---first-order transition systems---as a semantic
basis.  The so-called @emph{abstracting abstract machines} (AAM)
approach to abstract interpretation@~cite[aam] is a recipe for
transforming a machine semantics into an easily abstractable form.
There are a few essential elements to the transformation:

@itemlist[
@item{continuations are heap-allocated}
@item{variable bindings are heap-allocated}
@item{the range of the heap is changed from values to sets of values}
@item{heap update is interpreted as a join}
@item{heap dereference is interpreted as a non-deterministic choice}]

These transformations are semantics-preserving as the original and
derived machines operate in a lock-step correspondence.  But the real
value of the derived semantics stems from the fact that it's possible
to turn the derived machine into an abstract interpreter with two
simple steps:

@itemlist[
@item{bounding heap allocation to a finite set of addresses}
@item{widening base values to some abstract domain}
]

Moreover, the soundness of the resulting abstraction is self-evident
and easily proved.

The AAM approach has been used to build static analyzers for languages
such as Java, JavaScript, Racket, Coq, and Erlang.  It's been applied
to malware detection, contract verification, and ?.

Given the success of the AAM approach, it's natural to wonder what is
essential about the low-level machine basis of the semantics and
whether a similar approach is possible with a higher-level formulation
of the semantics such as a compositional evaluation function.

This paper shows that the essence of the AAM approach can be put on a
high-level semantic basis.  We show that compositional evaluators,
written in monadic style can express similar abstractions to that of
AAM.  Moreover, we show that the high-level semantics offers a number
of benefits not available to the machine model.  


Benefits of a definitional interpreter approach:

@itemlist[
@item{As we will see, the definitional interpreter approach is not
  formulated as a transformation on the semantics itself, but rather
  uses alternative notions of a monad to express the ``abstracting''
  transformations.  This means the concrete and abstract interpreters
  for a language can share large parts of their implementation; there
  is just one interpreter with a multiplicity of interpretations.}

@item{There is a rich body of work and many tools and techniques for
  constructing @emph{extensible} interpreters, all of which applies to
  high-level semantics, not machines.  By putting abstract
  interpretation for higher-order languages on a high-level semantic
  basis, we can bring these results to bear on the construction of
  extensible abstract interpreters.  In particular, we use @emph{monad
  transformers} to build re-usable components for mixing and matching
  the constiuent parts of an abstract interpreter.}

@item{Finally, using definitional interpreters for abstract
  interpretation satisfies an intellectual itch that asks whether it
  can be done at all.  In solving this technical challenge, we
  discover a pleasant surprise about the definitional interpreter
  approach: it is inherently ``pushdown,'' a property that has been
  the subject of several papers.  Under the interpreter approach, it
  comes for free.}  
]

@;{
@subsection{Notation and terminology}

Our technical development is carried out in Racket [Racket TR], a Lisp
dialect.  We assume a basic familiarity with definitional interpreters
in the style of Landin [Landin].
}

@section{A Definitional Interpreter}

We begin by first constructing a definitional interpreter for a small
but representative higher-order, functional language.  As our defining
language, we use an applicative subset of Racket, a dialect of
Scheme.@note{This choice is largely immaterial: any functional
language would do.}

The abstract syntax of the language is defined in @figure-ref{syntax};
it includes variables, numbers, unary and binary operations on
numbers, conditionals, @tt{letrec} expressions, functions, and
applications.

@figure["syntax" "Syntax"]{
@codeblock[#:keep-lang-line? #f]|{
  #lang racket
  E  ::= (vbl X)       ; Variable
         (num Number)  ; Number
         (lam X E)     ; Lambda
         (ifz E E E)   ; Conditional
         (op1 O1 E)    ; Unary primitive
         (op2 O2 E E)  ; Binary primitive
         (app E E)     ; Application
         (lrc X E E)   ; Letrec
  X  ::= Symbol        ; Variable name
  O1 ::= add1 ...      ; Unary operator
  O2 ::= + - ...       ; Binary operator
}|
}

@figure["pcf-eval" "Definitional interpreter"]{
@filebox[@racket[ev@]]{
@racketblock[
(define ((ev ev) e)
  (match e
    [(num n) (_return n)]
    [(vbl x)
     (do ρ ← _ask-env
         (_find (ρ x)))]    
    [(ifz e₀ e₁ e₂) 
     (do v  ← (ev e₀)
         z? ← (_zero? v)
         (ev (if z? e₁ e₂)))]
    [(op1 o e₀)
     (do v ← (ev e₀)
         (_δ o v))]   
    [(op2 o e₀ e₁)
     (do v₀ ← (ev e₀)
         v₁ ← (ev e₁)
         (_δ o v₀ v₁))]
    [(lrc f e₀ e₁) 
     (do ρ  ← _ask-env
         a  ← (_alloc f)
         ρ′ ≔ (ρ f a)
         (_ext a (cons e₀ ρ′))
         (_local-env ρ′
           (ev e₁)))]
    [(lam x e₀)
     (do ρ ← _ask-env
         (_return (cons (lam x e₀) ρ)))]
    [(app e₀ e₁)
     (do (cons (lam x e₂) ρ) ← (ev e₀)
         v₁ ← (ev e₁)
         a  ← (_alloc x)         
         (_ext a v₁)
         (_local-env (ρ x a) 
           (ev e₂)))]))]}}


The interpreter for the language is defined in @figure-ref{pcf-eval}.
At first glance, it has many conventional aspects:
@itemlist[
@item{it is compositionally defined by structural recursion on the syntax
  of expressions,}

@item{it represents functions with a closure data structure that pairs
  together the code with the environment in which a function
  definition was evaluated,}

@item{it is structured monadically and uses monad operations to interact
  with the environment and heap,}

@item{it relies on a helper function @racket[_δ] to interpret primitive operations.}
]

There are a few superficial aspects that deserve a quick note:
environments @racket[ρ] are finite maps and @racket[(ρ x)] denotes
@math{ρ(x)} while @racket[(ρ x a)] denotes @math{ρ[x↦a]}.  The
@racket[do]-notation is just shorthand for @racket[_bind], as usual:
@racketblock[
(do x ← e . r) ≡ (_bind e (λ (x) (do . r)))
(do x ≔ e . r) ≡ (let ((x e)) (do . r))
(do e . r) ≡ (_bind e (λ (_) (do . r)))
(do b) ≡ b
]

Finally, there are two unconvential aspects worth noting.  First, the
interpreter is written in an @emph{open recursive style}; the
evaluator does not call itself recursively, instead it takes as an
argument a function @racket[ev] which it calls in order to recur.  (We
have employed a bit of cuteness by naming the first parameter
@racket[ev], thereby shadowing the outer @racket[ev] and making
subsequent calls look like recursive calls.)  This is a standard
encoding for recursive functions in a setting without recursive
binding.  It is up to an external function, such as the Y-combinator,
to close the recursive loop.  As we will see, this open recursive form
will be crucial for interposition to collect information about the
intensional properties of evaluation.

Second, the code is clearly @emph{incomplete}.  There are a number
of free variables, noted in italics.  These free variables fall into
a few roles:
@itemize[

@item{providing the underlying monad of the interpreter:
@racket[_return] and @racket[_bind],}

@item{providing an interpretation of primitives: @racket[_δ] and
@racket[_zero?],}

@item{providing environment operations: @racket[_ask-env] for
retreiving the environment and @racket[_local-env] for installing an
environment,}

@item{providing store operations: @racket[_ext] for updating
the store, and @racket[_find] for dereferencing locations, and}

@item{a remaining operation for @racket[_alloc]ating store locations,
used to bind variables.}
]

Going forward, we make frequent use of sets of definitions involving
free variables, so we call such a collection a @emph{component}. We
assume components can be named (in this case, we've named the
component @racket[ev@], indicated by the box in the upper-right
corner) and linked together to eliminate free variables.@note{We use
Racket @emph{units}@~cite[flatt-pldi98] to model components in our
implementation.}

@figure["concrete-components" "Components for definitional interpreter"]{
@filebox[@racket[monad@]]{
@racketblock[
(define-monad 
  (ReaderT (FailT (StateT ID))))
]}
@filebox[@racket[δ@]]{
@racketblock[
(define (δ . ovs)
  (match ovs
    [(list 'add1 n)  (_return (add1 n))]
    [(list 'sub1 n)  (_return (sub1 n))]
    [(list '- n)     (_return (- n))]
    [(list '+ n₀ n₁) (_return (+ n₀ n₁))]
    [(list '- n₀ n₁) (_return (- n₀ n₁))]
    [(list '* n₀ n₁) (_return (* n₀ n₁))]
    [(list 'quotient n₀ n₁)
     (if (= 0 n₁)
         _fail
         (_return (quotient n₀ n₁)))]))

(define (zero? v)
  (_return (= 0 v)))
]}
@filebox[@racket[store@]]{
@racketblock[
(define (_find a)
  (do σ ← get-store
      (_return (σ a))))

(define (ext a v) 
  (_update-store (λ (σ) (σ a v))))
]}
@filebox[@racket[alloc@]]{
@racketblock[
(define (alloc x)
  (do σ ← _get-store
      (_return (size σ))))
]}
}

Let us now examine a set of components for completing the definitional
interpreter.  @figure-ref{concrete-components} gives the definition
for a series of components that complete the interpreter.  The first
and most magical component is @racket[monad@], which uses our
@racket[define-monad] macro to generate a set of bindings based on a
monad transformer stack.  For this interpreter, we use a failure monad
to model divide-by-zero errors, a state monad to model the store, and
a reader monad to model the environment.  The @racket[define-monad]
form generates bindings for @racket[return], @racket[bind],
@racket[ask-env], @racket[local-env], @racket[get-store] and
@racket[update-store]. 

We also add a @racket[mrun] operation for running computations, which
kicks off the computation by providing the empty environment and
store:
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
And the remaining operations are straightforward, too.  So the use of
monad transformers can be seen as a mere convenience, but as we will
see moving to more and more involved monad stacks, it's a useful one.

The @racket[δ@] component defines the interpretation of primitives,
which is given in terms of the underlying monad.  Finally the
@racket[alloc@] component provides a definition of @racket[alloc],
which fetches the store and uses its size to return a fresh address.

The @racket[store@] component defines the derived operations on stores
of @racket[find] and @racket[ext] for finding and extending the store
in terms of the monadic operations.

The only remaing pieces of the puzzle are a fixed-point combinator,
which is straightforward to define:
@racketblock[
(define ((fix f) x) ((f (fix f)) x))
]
And the main entry-point for the interpreter:
@racketblock[
(define (eval e) (mrun ((fix ev) e)))
]

By taking advantage of Racket's languages-as-libraries
features@~cite[tobin-hochstadt-pldi11], we can easily construct REPLs
for interacting with this interpreter.  Here are a few examples, which
make use of a concrete syntax for more succinctly writing expressions.
The identity function evaluates to an answer consisting of a closure
over the empty environment together with the empty store:
@interaction[#:eval the-pure-eval
  (λ (x) x)
]
Here's an example showing a non-empty environment and store:
@interaction[#:eval the-pure-eval
  ((λ (x) (λ (y) x)) 4)
]
Primitive operations work as expected:
@interaction[#:eval the-pure-eval
  (* (+ 3 4) 9)
]
And divide-by-zero errors result in failures:
@interaction[#:eval the-pure-eval
  (quotient 5 (- 3 3))
]
Because our monad stack places @racket[FailT] above @racket[StateT],
the answer includes the (empty) store at the point of the error.
Had we changed @racket[monad@] to use:
@racketblock[
(define-monad 
  (ReaderT (StateT (FailT ID))))
]
failures would not include the store:
@interaction[#:eval the-pure-eval-alt
  (quotient 5 (- 3 3))
]

At this point, we've defined a fairly run of the mill definitional
interpreter.  Despite these pedestrian beginnings, we essentially have
the complete skeleton for everything to come.  In particular, we will
reuse @racket[ev@] in all of the remaining interpreters.  Now let's do
something a bit more enchanting.

@section[#:tag "collecting"]{Collecting Variations}

The formal development of abstract interpretation often starts from a
so-called ``non-standard collecting semantics.''  A common form of
collecting semantics is a trace semantics, which collects streams of
states the interpreter reaches.  @Figure-ref{trace} shows the monad
stack for a tracing interpreter and a kind of ``mix-in'' for the
evaluator.  The monad stack adds @racket[WriterT] using
@racket[List], which provides a new operation named @racket[tell] for
writing items to the stream of reached states.  The @racket[ev-trace]
function is a wrapper around an underlying @racket[ev] function which
interposes itself between each recursive call by telling the current
state of the evaluator, that is the current expression, environment,
and store.  The top-level evaluation function is then:
@racketblock[
(define (eval e) 
  (mrun ((fix (ev-tell ev)) e)))
]

@figure["trace" "Trace collecting semantics"]{
@filebox[@racket[trace-monad@]]{
@racketblock[
(define-monad
  (ReaderT (FailT (StateT (WriterT List ID)))))
]}
@filebox[@racket[ev-tell@]]{
@racketblock[
(define (((ev-tell ev₀) ev) e)
  (do ρ ← _ask-env
      σ ← _get-store
      (_tell (list e ρ σ))
      ((ev₀ ev) e)))
]}}

Now when an expression is evaluated, we get the resulting answer and a
list of all the states seen by the evaluator, in the order in which
they were seen.  For example:
@interaction[#:eval the-trace-eval
  (* (+ 3 4) 9)
  ((λ (x) (λ (y) x)) 4)
]

Were we to swap @racket[List] with @racket[Set] in the monad stack, we
would obtain a @emph{reachable} state semantics, another common form
of collecting semantics, that loses the order and repetition of
states.

As another collecting semantics variant, we can also consider
collecting the @emph{dead code} in a program.  Here we use a monad
stack that has an addition state component (with operations named
@racket[put-dead] and @racket[get-dead]), which will the set of dead
expressions.  Initially, this will contain all of the subexpressions
of the program.  As the interpreter recurs through expressions, it
will remove them from the dead set.

@Figure-ref{dead} defines the monad stack for the dead code collecting
semantics and the @racket[ev-dead@] component which interposes itself
on an @racket[ev] function to remove the given subexpression before
recurring.  Since computing the dead code requires an outer wrapper
that sets the initial set of dead code to be all of the subexpressions
in the program, it requires a @racket[eval-dead@] component which
consumes a @emph{closed evaluator}, i.e. something of the form
@racket[(fix ev)].

Putting these pieces together, the dead code collecting semantics can
be defined as:
@racketblock[
(define (eval e)
  (mrun ((eval-dead (fix (ev-dead ev))) e)))
]

Running a program with the dead code interpreter produces an answer
and the set of expressions that were not evaluated during the running
of a program:
@interaction[#:eval the-dead-eval
(if0 0 1 2)
(* (+ 3 4) 9)
(λ (x) x)
(if0 (quotient 1 0) 2 3)]


@figure["dead" "Dead code collecting semantics"]{
@filebox[@racket[dead-monad@]]{
@racketblock[
(define-monad
  (ReaderT (StateT (StateT (FailT ID)))))
]}
@filebox[@racket[ev-dead@]]{
@racketblock[
(define (((ev-dead ev₀) ev) e)
  (do θ  ← _get-dead       
      (_put-dead (set-remove θ e))
      ((ev₀ ev) e)))
]}
@filebox[@racket[eval-dead@]]{
@racketblock[
(define ((eval-dead eval) e₀)
  (do (_put-dead (subexps e₀))
      (eval e₀)))
]}}

So our setup makes it easy not only to express the run of the mill
interpreter, but also different forms of collecting semantics.
Let us now start to look at abstractions.

@section[#:tag "base"]{Abstracting Base Values}

One of the things an abstract interpreter must do in order to become
decidable is to have some form of abstraction for the base types of
the language.  A very simple approach is to use a finite-element
abstract domain.  We can do this for our sole base type of numbers by
introducing a new kind of number, written @racket['N], which is an
abstract value that stands for all numbers.  Abstract values will be
introduced by alternative interpretation of the primitive operations,
given in @figure-ref{abs-delta}, which simply produces @racket['N] in
all cases.  Some care must be taken in the interpretation of
@racket['quotient] since if the denominator is an abstract value, the
result must include a failure since @racket[0] is in the set of values
abstracted by @racket['N].  This means that dividing a number by an
abstract value must produce @emph{two answers}: @racket['N] and
@racket['failure].  This is done by adding non-determinism to the
monad stack,

@racketblock[
  (ReaderT (FailT (StateT (NondetT ID))))
]
which provides a @racket[_mplus] operation for combining multiple
answers.  Non-determinism is also used in the implementation of
@racket[zero?], which returns both true and false on @racket['N].

@figure["abs-delta" "Abstracting primitive operations"]{
@filebox[@racket[δ^@]]{
@racketblock[
(define (δ . ovs)
  (match ovs
    [(list 'add1 n)  (_return 'N)]
    [(list 'sub1 n)  (_return 'N)]
    [(list '+ n₀ n₁) (_return 'N)]
    [(list '- n₀ n₁) (_return 'N)]
    [(list '* n₀ n₁) (_return 'N)]
    [(list 'quotient n₀ (? number? n₁))
     (if (= 0 n1) _fail (_return 'N))]
    [(list 'quotient n₀ n₁)
     (_mplus (return 'N) _fail)]))

(define (zero? v)
  (match v
    ['N (_mplus (_return #t) (_return #f))]
    [_  (_return (= 0 v))]))
]}}

By linking together the abstract variant of @racket[δ] and the monad
stack with non-determinism, we can obtain an evaluator that produces a
set of results:
@interaction[
  #:eval 
  (make-monadic-eval '(monad-nd@ δ-abs@ alloc@ state@ ev@ ev-trace@)
                     '(fix ev))
  (* (+ 3 4) 9)
  (quotient 5 (add1 2))
  (if0 (add1 0) 3 4)
]

If we were to link together the abstract variant of @racket[δ] with
the @emph{tracing} monad stack with non-determinism added in,
@racketblock[
(ReaderT (FailT (StateT 
  (WriterT List (NondetT ID)))))
]
we would get an evaluator that produces sets of traces:
@interaction[
  #:eval 
  (make-monadic-eval '(monad-trace-nd@ δ-abs@ alloc@ state@ ev@ ev-trace@)
                     '(fix (ev-trace ev)))

  (if0 (add1 0) 3 4)
]


It should be clear that the interepreter will only ever see a finite
set of numbers (including @racket['N]), but it's definitly not true
that the interpreter halts on all inputs.  Firstly, it's still
possible to generate an infinite number of closures.  Secondly,
there's no way for the interpreter to detect when it sees a loop.  To
make a terminating abstract interpreter requires tackling both.  Let's
look next at abstracting closures.

@section[#:tag "closures"]{Abstracting Closures}

Closures consist of code---a lambda term---and an environment---a
finite map from variables to addresses.  Since the set of lambda terms
and variables is bounded by the program text, it suffices to abstract
closures by abstracting the set of addresses.  Following the AAM
approach, we can do this by modifying the allocation function to
always produce elements drawn from a finite set.  In order to retain
soundness in the semantics, we will need to modify the store to map
addresses to @emph{sets} of values and model store update as a join
and dereference as a non-deterministic choice.

Any abstraction of the allocation function that produces a finite set
will do, but the choice of abstraction will determine the precision of
the resulting analysis.  A simple choice is to allocate variable
bindings by using a variable's name as its address.  This gives a
monomorphic, or 0CFA-like, abstraction.

@Figure-ref{0cfa-abs} shows an alternative component for finite
allocation that uses variables names as the notion of addresses and a
component for the derived operations @racket[find] and @racket[ext]
when the store uses a @emph{set} as its range.  The
@racket[for/monad+] form is just a convenience for combining a set of
computations with @racket[_mplus]; in other words, @racket[find]
returns @emph{all} of the values in the store at a given address.  The
@racket[ext] function joins whenever an address is already allocated,
otherwise it maps the address to a singleton set.

@figure["0cfa-abs" "Abstracting allocation: 0CFA"]{
@filebox[@racket[alloc^@]]{
@racketblock[
(define (alloc x)
  (_return x))
]}
@filebox[@racket[store-nd@]]{
@racketblock[
(define (find a)
  (do σ ← get-store
      (for/monad+ ([v (σ a)])
        (_return v))))

(define (ext a v)
  (_update-store
    (λ (σ) (σ a (if (∈ a σ) 
                    (set-add (σ a) v) 
                    (set v))))))
]}
}

By linking these components together with the same monad stack from
@Secref{base}, we obtain an interpreter that loses precision whenever
variables are bound to multiple values.  For example, this program
binds @racket[x] to both @racket[0] and @racket[1] and therefore
produces both answers when run:
@interaction[#:eval the-0cfa-eval
  (let f (λ (x) x)
    (let _ (f 0) (f 1)))]

We've now taken care of making a sound, finite abstraction of the
space of all closures that arise during evaluation.  It would seem we
are very close to having a sound, total abstract interpretation
function.

@section[#:tag "cache"]{Detecting Cycles with a Cache}

At this point, it's worth observing that the interpreter obtained by
linking together @racket[δ^@] and @racket[alloc^@] components will
only ever visit a finite number of states for a given program.  The
state consists of an expression, environment, and store mapping
addresses to sets of values.  To see that this is a finite set is
pretty straightforward: expressions (in the given program) are finite,
environments are maps from variables (again, finite in a program) to
address.  The addresses are finite thanks to @racket[alloc^], so
environments are finite.  The store maps addresses (finite by
@racket[alloc^]) to sets of values.  The values are base values, which
are finite by @racket[δ^], or closures which consist of expressions
(in the given program) and an environment, which we just seen are
finite.  Since the elements of the sets are finite, the sets
themselves are finite, and therefore stores, and finally states, are
all finite sets.

The problem is that while the interpreter only ever see a finite set
of inputs, it @emph{doesn't know it}.  So even a simple loop will
cause the interpreter to diverge:

@interaction[#:eval the-0cfa-eval
(rec f (λ (x) (f x)) (f 0))
]

To solve this problem, let's introduce a notion of a @emph{cache}
which is a mapping from states to sets of value, store pairs.  The basic idea is
that we will use the cache to do a form of co-inductive programming.
While evaluating a state @racket[ς] for the first time, we may at some
point be asked to evaluate exactly @racket[ς] again.  Should this
happen, we can return the empty set of results.  We will use the cache
to track the encountered states and the results they produce.  By
maintaining the cache, we avoid the possibility of diverging.

@figure["ev-cache0" "Caching, first attempt"]{
@filebox[@racket[ev-cache′@]]{
@racketblock[
(define (((ev-cache ev₀) ev) e)
  (do ρ ← _ask-env
      σ ← _get-store
      ς ≔ (list e ρ σ)
      Σ ← _get-$
      (if (∈ ς Σ)
          (for/monad+ ([v×σ (Σ ς)])
            (do (_put-store (cdr v×σ))
                (_return (car v×σ))))
          (do (_put-$ (Σ ς ∅))
              v  ← ((ev₀ ev) e)
              (_update-$ 
                (λ (Σ) 
                  (Σ ς (set-add (Σ ς)
                                (cons v σ)))))
              (_return v)))))
]}}

We use the following monad stack, which adds a ``cache'' component,
which will be a finite map from states to sets of values:
@racketblock[
(ReaderT (FailT (StateT (NondetT (StateT+ ID)))))
]

The @racket[StateT+] monad transformer provides operations
@racket[get-$] and @racket[update-$] for getting and updating the
cache, respectively. It joins its finite maps by union of the range
when @racket[mplus] is called, because it cannot defer to an
underlying monoid as the outer @racket[StateT] does with
@racket[NondetT].

@Figure-ref{ev-cache0} gives an @racket[ev]-wrapper that interposes
itself on each recursive call to do the following steps:
@nested[#:style 'inset]{Check if the
current state is in the cache.  If it's in the cache, return all the
results given in the cache.  If it's not, set the cache for the
current state to the empty set, evaluate the expression,
add the resulting value to the cache for the state, and
return the result.}

We can now define an evaluation function that mixes in
@racket[ev-cache]:
@racketblock[
(define (eval e)
  (mrun ((fix (ev-cache ev)) e)))
]

If we were to link this together with @racket[alloc@] and @racket[δ@],
we'd obtain a concrete interpreter that either 1) produces the empty
set because it encountered a loop, 2) produces a singleton result, or
3) diverges because it encounters an infinite set of states.  But if
we were to link this together with @racket[alloc^@] and @racket[δ^@],
we'd obtain an abstract interpreter that is @emph{total}: it
terminates on all inputs.

@;{@note{Unfortunately, without a smarter allocation strategy, this
will never happen because looping programs allocate an inifinite
amount of memory. This is possible to fix, but would be a
distraction.}}


To see why this, observe that for a given program there only a finite
set of possible caches.  We have already seen that there are a finite
set of states and values, so it follows that there are only a finite
set of maps from states to sets of values.  Now notice that on each
recursive call, either the state is in the cache and it returns
immediately, or the cache grows.  So programs simply cannot run
forever because that would imply the cache would grow forever.

It should be easy to see that if evaluating a state @racket[ς] requires
recursively evaluating that same state, it will now produce the empty
set since the cache will be updated to map @racket[ς] to @racket[∅] before
proceeding to the sub-expressions.

We can now see that the caching abstract interpreter halts on programs
that loop (for simplicity, the cache and store are omitted from the
printed results):
@interaction[#:eval the-simple-cache-eval
(rec f (λ (x) (f x)) (f 0))
]
This accomplishes the goal of terminating on this example, and it even
gives the right answer---the empty set---since this program produces
no results.

It also works for recursive functions that terminate in the concrete,
but have loops once abstracted:
@interaction[#:eval the-simple-cache-eval
(rec fact (λ (n)
           (if0 n 1 (* n (fact (sub1 n)))))
  (fact 5))
]

It may seem we've accomplished our goal of making a sound and
decidable abstract interpreter.  However this approach is broken in
general: it is not sound in the presence of abstraction.  The problem
here is that when the interpreter reaches ``the same'' state it has
seen before, what we mean by ``the same'' in the presence of
abstraction is subtle.  For example, imagine evaluating a function
application of some function @racket[f] to an abstract value
@racket['N].  Suppose in evaluating this application we encounter
another application of @racket[f] to @racket['N].  Is it the same
application?  Well, yes and no.  It is the same @emph{abstract} state,
however the abstract state stands for a set of concrete states; in
this case, the application of @racket[f] to all numbers.  So there are
states stood for in the abstraction that are equal @emph{and} not
equal.  In other words, in the presence of abstraction, when a loop is
detected, there @emph{may} be a loop in the concrete interpretation.
Our naive loop detection set-up however is assuming there @emph{must}
be a loop.

We can demonstrate the problem with a simple counter-example to
soundness:

@interaction[#:eval the-simple-cache-eval
(rec f (λ (x) 
         (if0 x 0 (if0 (f (sub1 x)) 2 3)))
   (f (add1 0)))
]

Concretely, this program always returns @racket[2], however with the
combination of loop detection and abstraction determines that this
program always produces @racket[0].  That's clearly unsound.

@section[#:tag "fixing-cache"]{Fixing the Cache}

The basic problem with the caching solution of @secref{cache} is that
it cuts short the exploration of the program's behavior.  In the
soundness counter-example, the inner call to @racket[f] is present in
the cache so neither branch of the conditional is taken; it is at this
point of bottoming out that we determine @racket[f] may return
@racket[0].  Of course, now we know that the conditional should have
take the true branch since @racket[0] could be returned, but it's too
late: the program has terminated.

To restore soundness, what we need to do is somehow @emph{iterate} the
interpreter so that we can re-explore the behavior knowing that
@racket[f] may produce @racket[0].  A first thought may be to do a
complete evaluation of the program, take the resulting cache, and then
feed that in as the initial cache for a re-evaluation of the program.
But there's an obvious problem... doing so would result in a cache hit
and the saved results would be returned immediately without exploring
any new behavior.

The real solution is that we want to use the prior evaluation's cache
as a kind of co-inductive hypothesis: it's only when we detect a loop
that we want to produce all of the results stored in the prior cache.
This suggests a two-cache system in which the prior cache is only used
when initializing the local cache.  In other words, we want to use the
prior cache entry in the place of @racket[∅].  When iterating the
interpreter, we always start from an empty local cache and fall back
on the prior cache results when initializing the cache entry before
making a recursive call.  Since the prior cache is never written to,
we can model the prior cache as a reader monad and add it to the
stack:
@racketblock[
(ReaderT (FailT (StateT (NondetT 
  (ReaderT @code:comment{the prior cache}
    (StateT+ ID))))))
]

The revised @racket[ev-cache@] component is given in
@figure-ref{ev-cache}, which uses the @racket[_ask-⊥] operation to
retreive the prior cache.  If the prior cache is empty, this code
degenerates into exactly what was given in @figure-ref{ev-cache0}.

@figure["ev-cache" "Caching, with fall-back to prior"]{
@filebox[@racket[ev-cache@]]{
@racketblock[
(define (((ev-cache ev₀) ev) e)
  (do ρ ← _ask-env
      σ ← _get-store
      ς ≔ (list e ρ σ)
      Σ ← _get-$
      (if (∈ ς Σ)
          (for/monad+ ([v×σ (Σ ς)])
            (do (_put-store (cdr v×σ))
                (_return (car v×σ))))
          (do Σ⊥ ← _ask-⊥
              @code:comment{initialize to prior, if exists}
              (_put-$ (Σ ς (if (∈ ς Σ⊥) (Σ⊥ ς) ∅)))
              v  ← ((ev₀ ev) e)
              (_update-$ 
                (λ (Σ) 
                  (Σ ς (set-add (Σ ς)
                                (cons v σ)))))
              (_return v)))))
]}}

We are left with two remaining problems; we need to figure out: 1) how
to pipe the cache from one run of the interpreter into the next and 2)
when to stop.  The answer to both is given in @figure-ref{cache-fix}.

The @racket[fix-cache] function takes a closed evaluator,
just like @racket[eval-dead] from @secref{collecting},
i.e. something of the form @racket[(fix ev)].  It iteratively runs the
evaluator.  Each run of the evaluator resets the ``local'' cache to
empty and uses the cache of the previous run as it's fallback cache
(initially it's empty).  The computation stops when a least
fixed-point in the cache has been reached, that is, when running the
evaluator with a prior gives no changes in the resulting cache.  At
that point, the result is returned.

@figure["cache-fix" "Finding fixed-points in the cache"]{
@filebox[@racket[fix-cache@]]{
@racketblock[
(define ((fix-cache eval) e)  
  (do ρ ← _ask-env
      σ ← _get-store
      ς ≔ (list e ρ σ)
      (mlfp (λ (Σ) (do (_put-$ ∅-map)
                       (_put-store σ)
                       (_local-⊥ Σ (eval e))
                       _get-$)))
      Σ ← _get-$
      (for/monad+ ([v×σ (Σ ς)])
        (do (_put-store (cdr v×σ))
            (_return (car v×σ))))))

(define (mlfp f)
  (let loop ([x ∅-map])
    (do x′ ← (f x)
        (if (equal? x′ x)
            (return (void))
            (loop x′)))))
]}}

With these peices in place, we can construct an interpreter as:
@racketblock[
(define (eval e)
  (mrun ((fix-cache (fix (ev-cache ev))) e)))
]
When linked with @racket[δ^] and @racket[alloc^], this interpreter is
a sound, computable abstraction of the original definitional
interpreter.  Note that the iterated evaluator always terminates: the
cache resulting from each run of the evaluator contains @emph{at
least} as much information as the prior cache, each run of the
evaluator terminates, so the iterated evaluator terminates by the same
principle as before: the cache monotonically grows and is finite in
size.

We have thus achieved our goal and can confirm it gives
the expected answers on the previous examples:

@interaction[#:eval the-pdcfa-eval
(rec f (λ (x) (f x)) (f 0))
(rec fact (λ (n)
           (if0 n 1 (* n (fact (sub1 n)))))
  (fact 5))
(rec f (λ (x) 
         (if0 x 0 (if0 (f (sub1 x)) 2 3)))
   (f (add1 0)))

]
Let us now take stock of what we've got.

@include-section{reynolds.scrbl}
@include-section{store-widen.scrbl}

@section[#:tag "symbolic"]{Symbolic Execution and Path-Sensitive Verification}

We present an extension to the monad stack and metafunctions
that gives rise to a symbolic execution@~cite[king-76],
then show how abstractions discussed in previous sections
can be applied to enforce termination,
turning a traditional symbolic execution into a path-sensitive
verification.

@subsection{Symbolic Execution}
@Figure-ref{symbolic} shows the units needed to turn the existing interpreter
into a symbolic executor, in addition to adding symbolic numbers @racket[(sym X)]
to the language syntax.
Primitives such as @racket['quotient] now may also take as input
and return symbolic values.
As standard, symbolic execution employs a path-condition
accumulating assumptions made at each branch,
allowing the elimination of infeasible paths and construction of test cases.
We represent the path-condition @racket[φ] as a set of symbolic values
known to have evaluated to @racket[0].
This set is another state component provided by @racket[StateT].
Monadic operations @racket[_get-path-cond]
and @racket[_refine] reference and update the path-condition.
Metafunction @racket[_zero?] works similarly to the concrete counterpart,
but also uses the path-condition to prove that some symbolic numbers
are definitely @racket[0] or non-@racket[0].
In case of uncertainty, @racket[_zero?] returns both answers
besides refining the path-condition with the assumption made.
Operator @racket['¬] represents negation in our language.

In the following example, the symbolic executor recognizes that
result @racket[3] and division-by-0 error are not feasible:
@interaction[
  #:eval the-symbolic-eval 
  (if0 'x (if0 'x 2 3) '(quotient 5 x))
]

A scaled up symbolic executor can have @racket[_zero?] calling out
to an SMT solver for interesting arithmetics,
and extend the language with symbolic functions
and blame semantics for sound higher-order symbolic
execution@~cite[vanhorn-oopsla12 nguyen-pldi15].

@figure["symbolic" "Symbolic execution variant"]{
@codeblock[#:keep-lang-line? #f]|{
  #lang racket
  E  ::= ... (sym X)  ; Symbolic number
}|
@filebox[@racket[symbolic-monad@]]{
@racketblock[
(define-monad
  (ReaderT (FailT (StateT (StateT (NondetT ID))))))
]}
@filebox[@racket[ev-symbolic@]]{
@racketblock[
(define (((ev-symbolic ev₀) ev) e)
  (match e
    [(sym x) (_return x)]
    [e       ((ev₀ ev) e)]))
]}
@filebox[@racket[δ-symbolic@]]{
@racketblock[
(define (δ . ovs)
  (match ovs
    ... ; TODO can't put comment in here...
    [(list 'quotient v₀ v₁)
     (do z? ← (_zero? v₁)
         (cond
          [z? _fail]
          [(and (number? v₀) (number? v₁))
           (return (quotient v₀ v₁))]
          [else
           (return `(quotient ,v₀ ,v₁))]))]
    [(list '¬ 0) 1]
    ... ; TODO can't put comment in here...
    ))
(define (zero? v)
  (do φ ← _get-path-cond
      (match v
        [(? number? n) (return (= 0 n))]
        [v #:when (∈ v φ) (return #t)]
        [v #:when (∈ v `(¬ ,v)) (return #f)]
        [`(¬ ,v′) (do a ← (zero? v′)
                     (not a))]
        [v (mplus (do (_refine v)
                      (return #t))
                  (do (_refine `(¬ ,v))
                      (return #f)))])))
]}}

@subsection{From Symbolic Execution to Verification}

Traditional symbolic executors mainly aim to find bugs
and provide no termination guarantee.
We can apply abstracting units presented in previous sections,
namely base value widening (@secref{base}), finite allocation (@secref{closures}),
caching and fixing (@secref{cache} and @secref{fixing-cache}) to turn
a symbolic execution into a sound, path-sensitive program verification.

Operations on symbolic values introduce a new source of infinite configurations
by building up new symbolic values.
We therefore straightforwardly widen a symbolic value to the abstract
number @racket['N] when it shares an address with a different number.
@Figure-ref{symbolic-widen} shows extension to @racket[_δ] and @racket[_zero?]
in the presence of @racket['N].
The different treatments of @racket['N] and symbolic values
clarifies that abstract values are not symbolic values:
the former stands for a set of multiple values,
whereas the latter stands for an single unknown value.
Tests on abstract number @racket['N] do not strengthen the path-condition.
It is unsound to accumulate any assumption about @racket['N].

@figure["symbolic-widen" "Symbolic execution with abstract numbers"]{
@filebox[@racket[δ-symbolic@]]{
@racketblock[
(define (δ . ovs)
  (match ovs
    ... ; TODO can't put comment in here...
    [(list 'quotient v₀ v₁)
     (do z? ← (_zero? v₁)
         (cond
          [z? _fail]
          [else
           (match (list v₀ v₁)
            [(list (? number? n₀) (? number? n₁))
             (return (quotient n₀ n₁))]
            [(list _ ... 'N _ ...)
             (return 'N)]
            [(list v₀ v₁)
             (return `(quotient ,v₀ ,v₁))])]))]
    ... ; TODO can't put comment in here...
    ))
(define (zero? v)
  (do φ ← _get-path-cond
      (match v
        ['N (mplus (return #t) (return #f))]
        ...)))
]}}


@;{
(eval (lrc 'f (lam 'x
                   (ifz (vbl 'x)
                        (num 0)
                        (ifz (app (vbl 'f)
                                  (op1 'sub1 (sym 'x)))
                             (num 7)
                             (num 9))))
           (app (vbl 'f)
                (op1 'sub1 (num 2)))))
}


@;{

@section{Non-standard Semantics}

It is common in the abstract interpretation literature to consider an
idealized, non-standard semantics from which to abstract.  Often these
semantics take the form of a collecting semantics that collect either
traces or reachable states.

In this section, we demonstrate that such semantics can be realized as
implementations of the monadic operations for the interpreter given in
the previous section.

@subsection{Trace semantics}

A trace semantics not only computes the result of a computation but
also a string of intermediate states that deliver that result starting
from the initial program.  This is trivial to construct for a
small-step reduction relation, but less obvious for a compositional
evaluation function.  Crucially, by parameterizing our evaluator over
the @racket[rec^] signature, it is possible to observe each call to
the evaluator and therefore we can construct an implementation of
@racket[rec^] that records each call.


@centered{
@racket[
(link eval-trace@ ev!@ δ@ env-sto@ sto@)
]}

@examples[#:eval the-trace-eval
(* 2 2)
]


@figure["trace-eval" "Trace evaluator"]{
@filebox[@racket[eval-trace@]]{
@racketblock[
(import ev^)
(export eval^ unit^ bind^ rec^ err^)

(define (eval e)
  (((rec e (hash)) (hash)) empty))

(define (((rec e r) s) t)
  (((_ev e r) s) (cons (list e r s) t)))

(define (((unit v) s) t)
  (cons (cons v s) t))

(define (((err) s) t)
  (cons (cons 'err s) t))

(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t)
     (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
]
}}


@subsection{Reachable state semantics}

The reachable state semantics is just an abstraction of the trace
semantics that ignores the ordering on states.  To implement such a
semantics, we simply collect a set of evaluator calls rather than a
sequence.

@examples[#:eval the-reachable-eval
(* 2 2)
]

@subsection{Co-inductive semantics}

In this section, we construct an interpreter that detects loops in
evaluation.  If a program terminates, it produces a singleton set; if
the evaluator loops, it produces the empty set.  (We choose a set
intrepretation rather than an option in anticipation of the subsequent
development of a non-deterministic semantics.)


@section{Symbolic Evaluation}

We have two primary motivations for extending the definitional
evaluator to interpret @emph{symbolic} values:

@itemlist[#:style 'numbered
@item{Abstracting base values

When designing an abstract interpreter, an important task is
abstracting the base values of the language.  Often, this abstraction
can be expressed in terms of giving a semantics to symbolic values.
As an example, our model's only base type is number, which
includes an inifinite set of values.  We can give a finite abstraction
of this set by considering an abstract number as being either one of
the set of numbers that @emph{literally} appear in a given program, or
the symbolic value ``number,'' written @racket['N].  The addition of
the @racket['N] value requires extending the @racket[_δ]
function to interpret operations over arguments such as @racket['N].
}

@item{Modularity

An important, but typically missing, aspect of program analysis is
being able to analyze program modules with unknown or parametric
components.  This can be handled by treating unknown values as
symbolic.
}
]

We extend the language to include symbolic terms.  A symbolic term evaluates to
its symbol, which we take to stand for the value of an arbitrary
(well-typed) program.
@codeblock[#:keep-lang-line? #f]|{
#lang racket
;;  E = ...
;;    | (sym Symbol)      Symbolic term
;;  V = ...
;;    | Sexpr             Symbolic value

;; Sexpr = Symbol
;;       | (list V V)
;;       | (list O1 V)
;;       | (list O2 V V)

(define (eval e r s)
  (match e
    [(sym x) (_unit (cons x s))]
    ...))


}|



@section{Conclusion}

@centered{
@url{https://github.com/dvanhorn/monadic-eval}}

}

@include-section{try-it.scrbl}
@include-section{related-work.scrbl}


@;{
@bold{Acknowledgments}: Sam Tobin-Hochstadt, J. Ian Johnson, Olivier Danvy.
}

@(generate-bibliography)
