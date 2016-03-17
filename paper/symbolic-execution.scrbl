#lang scribble/manual
@(require scribble/eval
	  scriblib/figure
	  "bib.rkt"
          "evals.rkt")

@title[#:tag "symbolic"]{Symbolic Execution and Path-Sensitive Verification}

As a final exercise in abstract interpretation component engineering,
we develop a monadic view of symbolic execution.

We present an extension to the monad stack and metafunctions
that gives rise to a symbolic executor@~cite[king-76],
then show how abstractions discussed in previous sections
can be applied to enforce termination,
turning a traditional symbolic execution into a path-sensitive
verification engine.

@section{Symbolic Execution}
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
  (if0 'x (if0 'x 2 3) (quotient 5 'x))
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
        [v #:when (∈ `(¬ ,v) φ) (return #f)]
        [`(¬ ,v′) (do a ← (zero? v′)
                      (return (not a)))]
        [v (mplus (do (_refine v)
                      (return #t))
                  (do (_refine `(¬ ,v))
                      (return #f)))])))
]}}

@section{From Symbolic Execution to Verification}

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
