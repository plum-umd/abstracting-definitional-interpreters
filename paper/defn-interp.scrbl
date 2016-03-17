#lang scribble/manual
@(require scribble/eval
          scriblib/figure
          scriblib/footnote
          "bib.rkt"
          "evals.rkt")
           
@title{A Definitional Interpreter}

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
  with the environment and store,}

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
