#lang scribble/acmart @acmlarge

@(require scriblib/figure 
          scribble/manual 
          scribble/eval
          "evals.rkt"
          "bib.rkt")

@title[#:tag "s:symbolic"]{Symbolic execution}

@figure["f:symbolic" "Symbolic Execution Variant"]{
@filebox[@racket[symbolic-monad@]]{
@racketblock[
(define-monad (ReaderT (FailT (StateT (StateT (NondetT ID))))))]}
@filebox[@racket[ev-symbolic@]]{
@racketblock[
(define (((ev-symbolic ev₀) ev) e)
  (match e [(sym x) (return x)]
           [e       ((ev₀ ev) e)]))]}
@filebox[@racket[δ-symbolic@]]{
@racketblock[
(define (δ o n₀ n₁)
  (match* (o n₀ n₁)
    [('/ n₀ n₁) (do z? ← (zero? n₁)
                    (cond [z? fail]
                          [(and (num? n₀) (num? n₁)) (return (/ n₀ n₁))]
                          [else (return `(/ ,n₀ ,n₁))]))] ... ))
(define (zero? v)
  (do φ ← get-path-cond
      (match v
        [(? num? n)             (return (= 0 n))]
        [v #:when (∈ v φ)       (return #t)]
        [v #:when (∈ `(¬ ,v) φ) (return #f)]
        [v (mplus (do (refine v)       (return #t))
                  (do (refine `(¬ ,v)) (return #f)))])))]}}


@figure["f:symbolic-widen" "Symbolic Execution with Abstract Numbers"]{
@filebox[@racket[δ^-symbolic@]]{
@racketblock[
(define (δ o n₀ n₁)
  (match* (o n₀ n₁)
    [('/ n₀ n₁) (do z? ← (zero? n₁)
                    (cond [z? fail]
                          [(member 'N (list n₀ n₁)) (return 'N)]
                          ... ))]
    ... ))
(define (zero? v)
  (do φ ← get-path-cond
      (match v ['N (mplus (return #t) (return #f))] ... )))]}}


In this section, we carry out another---this time more
involved---example that shows how to instantiate our definitional
abstract interpreter to obtain a symbolic execution engine that
performs sound program verification.  This serves to demonstrate the
range of the approach, capturing forms of analysis typically
considered fairly dissimilar.

First, we describe the monad stack and metafunctions that implement a
symbolic executor @~cite{dvanhorn:King1976Symbolic}, then we show how
abstractions discussed in previous sections can be applied to enforce
termination, turning a traditional symbolic execution into a
path-sensitive verification engine.

To support symbolic execution, the syntax of the language is extended to
include symbolic numbers:
@racketblock[
FIXME
]
@;{
\begin{alignat*}{4}
   e ∈ &&\mathrel{}     exp ⩴ &\mathrel{} … ∣ 𝔥⸨(sym⸩\ x𝔥⸨)⸩ &\hspace{1em} [⦑\emph{symbolic number}⦒]
\\ ε ∈ &&\mathrel{}    pexp ⩴ &\mathrel{} e ∣ ¬e             &\hspace{1em} [⦑\emph{path expression}⦒]
\\ φ ∈ &&\mathrel{}    pcon ≔ &\mathrel{} ℘(pexp)   &\hspace{1em} [⦑\emph{path condition}⦒]
\end{alignat*}
}
@Figure-ref{f:symbolic-widen} shows the units needed to turn the
existing interpreter into a symbolic executor. Primitives such as
@racket['/] now also take as input and return symbolic values. As
standard, symbolic execution employs a path-condition accumulating
assumptions made at each branch, allowing the elimination of provably
infeasible paths and construction of test cases. We represent the
path-condition @racket[φ] as a set of symbolic values or their
negations.  If @racket[e] is in @racket[φ], @racket[e] is assumed to
evaluate to @racket[0]; if @racket[¬ e] is in @racket[φ], @racket[e]
is assumed to evaluate to non-@racket[0].  This set is another state
component provided by @racket[StateT] in the monad transformer
stack. Monadic operations @racket[get-path-cond] and @racket[refine]
reference and update the path-condition. The metafunction
@racket[zero?]  works similarly to the concrete counterpart, but also
uses the path-condition to prove that some symbolic numbers are
definitely @racket[0] or non-@racket[0]. In case of uncertainty,
@racket[zero?] returns both answers instead of refining the
path-condition with the assumption made.

In the following example, the symbolic executor recognizes that result
@racket[3] and division-by-0 error are not feasible:
@interaction[#:eval the-symbolic-eval
(if0 'x (if0 'x 2 3) (quotient 5 'x))
]
A scaled up symbolic executor could implement @racket[zero?] by
calling out to an SMT solver for more interesting reasoning about
arithmetic, or extend the language with symbolic functions and blame
semantics for sound higher-order symbolic execution, essentially
recreating a pushdown variant of Nguyễn et
al.@~cite{dvanhorn:TobinHochstadt2012Higherorder
dvanhorn:Nguyen2014Soft dvanhorn:Nguyen2015Relatively}.

Traditional symbolic executors mainly aim to find bugs and do not
provide a termination guarantee. However, when we apply to this
symbolic executor the finite abstractions presented in previous
sections, namely base value widening and finite allocation
(@secref{s:base}), and caching and fixing (@secref{s:cache}), we turn
the symbolic execution into a sound, path-sensitive program
verification engine.

There is one wrinkle, which is that operations on symbolic values
introduce a new source of unboundness in the state-space, because the
space of symbolic values is not finite. A simple strategy to ensure
termination is to widen a symbolic value to the abstract number ⸨'N⸩
when it shares an address with a different number, similarly to the
precision-preserving abstraction from
@secref{s:alt-abstraction}. @Figure-ref{f:symbolic-widen} shows
extension to @racket[δ] and @racket[zero?] in the presence of
@racket['N]. The different treatments of @racket['N] and symbolic
values clarifies that abstract values are not symbolic values: the
former stands for a set of multiple values, whereas the latter stands
for an single unknown value. Tests on abstract number @racket['N] do
not strengthen the path-condition; it is unsound to accumulate any
assumption about @racket['N].
