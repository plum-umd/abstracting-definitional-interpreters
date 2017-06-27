#lang scribble/acmart @acmlarge
@(require scriblib/figure 
          scribble/manual 
          scriblib/footnote
          scribble/eval
          racket/match
          "evals.rkt"
          "bib.rkt")

@title[#:tag "s:cache"]{Caching and Finding Fixed-points}

At this point, the interpreter obtained by linking together
@racket[monad^@], @racket[δ^@], @racket[alloc^@] and
@racket[store-nd@] components will only ever visit a finite number of
configurations for a given program. A configuration (@racket[ς])
consists of an expression (@racket[e]), environment (@racket[ρ]) and
store (@racket[σ]). This configuration is finite because: expressions
are finite in the given program; environments are maps from variables
(again, finite in the program) to addresses; the addresses are finite
thanks to @racket[alloc^]; the store maps addresses to sets of values;
base values are abstracted to a finite set by @racket[δ^]; and
closures consist of an expression and environment, which are both
finite.

Although the interpreter will only ever see a finite set of inputs, it
@emph{doesn't know it}.  A simple loop will cause the interpreter to
diverge:
@interaction[#:eval the-0cfa-eval
((rec f (λ (x) (f x))) 0)
]
To solve this problem, we introduce a @emph{cache} (@racket[$in]) as
input to the algorithm, which maps from configurations (@racket[ς]) to
sets of value-and-store pairs (@racket[v×σ]). When a configuration is
reached for the second time, rather than re-evaluating the expression
and entering an infinite loop, the result is looked up from
@racket[$in], which acts as an oracle. It is important that the cache
is used co-inductively: it is only safe to use @racket[in] as an
oracle so long as some progress has been made first.

The results of evaluation are then stored in an output cache
(@racket[$out]), which after the end of evaluation is “more defined”
than the input cache (@racket[$in]), again following a co-inductive
argument. The least fixed-point @racket[$⁺] of an evaluator which
transforms an oracle @racket[$in] and outputs a more defined oracle
$racket[out] is then a sound approximation of the program, because it
over-approximates all finite unrollings of the unfixed evaluator.

The co-inductive caching algorithm is shown in @Figure-ref{f:caching},
along with the monad transformer stack @racket[monad-cache@] which has
two new components: @racket[ReaderT] for the input cache @racket[$in],
and @racket[StateT+] for the output cache @racket[$out]. We use a
@racket[StateT+] instead of @racket[WriterT] monad transformer in the
output cache so it can double as tracking the set of seen states. The
@racket[+] in @racket[StateT+] signifies that caches for multiple
non-deterministic branches will be merged automatically, producing a
set of results and a single cache, rather than a set of results paired
with individual caches.

@figure["f:caching" "Co-inductive Caching Algorithm"]{
@filebox[@racket[monad-cache@]]{
@racketblock[
(define-monad 
  (ReaderT (FailT (StateT (NondetT (ReaderT (StateT+ ID)))))))]}
@filebox[@racket[ev-cache@]]{
@racketblock[
(define (((ev-cache ev₀) ev) e)
  (do ρ ← ask-env
      σ ← get-store
      ς ≔ (list e ρ σ)
      $⸢out⸣ ← get-cache-out
      (if (∈ ς $⸢out⸣)
          (for/monad+ ([v×σ ($⸢out⸣ ς)])
            (do (put-store (cdr v×σ))
                (return (car v×σ))))
          (do $⸢in⸣ ← ask-cache-in
              v×σ₀  ≔ (if (∈ ς $⸢in⸣) ($⸢in⸣ ς) ∅)
              (put-cache-out ($⸢out⸣ ς v×σ₀))
              v ← ((ev₀ ev) e)
              σ′ ← get-store
              v×σ′ ≔ (cons v σ′)
              (update-cache-out 
               (λ ($⸢out⸣) ($⸢out⸣ ς (set-add ($⸢out⸣ ς) v×σ′))))
              (return v)))))]}}

In the algorithm, when a configuration @racket[ς] is first
encountered, we place an entry in the output cache mapping @racket[ς]
to @racket[($in ς)], which is the “oracle” result. Also, whenever we
finish computing the result @racket[v×σ′] of evaluating a
configuration @racket[ς], we place an entry in the output cache
mapping @racket[ς] to @racket[v×σ′]. Finally, whenever we reach a
configuration @racket[ς] for which a mapping in the output cache
exists, we use it immediately, @racket[return]ing each result using
the @racket[for/monad+] iterator. Therefore, every “cache hit” on
@racket[$out] is in one of two possible states: 1) we have already
seen the configuration, and the result is the oracle result, as
desired; or 2) we have already computed the “improved” result
(w.r.t. the oracle), and need not recompute it.

To compute the least fixed-point @racket[$⁺] for the evaluator
@racket[ev-cache] we perform a standard Kleene fixed-point iteration
starting from the empty map, the bottom element for the cache, as
shown in @Figure-ref{f:fixing}.

@figure["f:fixing" "Finding Fixed-Points in the Cache"]{
@filebox[@racket[fix-cache@]]{
@racketblock[
(define ((fix-cache eval) e)  
  (do ρ ← ask-env  σ ← get-store
      ς ≔ (list e ρ σ)
      $⁺ ← (mlfp (λ ($) (do (put-cache-out ∅)
                            (put-store σ)
                            (local-cache-in $ (eval e))
                            get-cache-out)))
      (for/monad+ ([v×σ ($⁺ ς)])
        (do (put-store (cdr v×σ))
            (return (car v×σ))))))
(define (mlfp f)
  (let loop ([x ∅])
    (do x′ ← (f x)
        (if (equal? x′ x) (return x) (loop x′)))))]}}

The algorithm runs the caching evaluator @racket[eval] on the given
program @racket[e] from the initial environment and store. This is
done inside of @racket[mlfp], a monadic least fixed-point
finder. After finding the least fixed-point, the final values and
store for the initial configuration @racket[ς] are extracted and
returned.

Termination of the least fixed-point is justified by the monotonicity of the
evaluator (it always returns an “improved” oracle), and the finite domain of
the cache, which maps abstract configurations to pairs of values and stores,
all of which are finite.

With these pieces in place we construct a complete interpreter:
@racketblock[
(define (eval e) (mrun ((fix-cache (fix (ev-cache ev))) e)))
]
When linked with @racket[δ^] and @racket[alloc^], this abstract
interpreter is sound and computable, as demonstrated on the following
examples:
@interaction[#:eval the-pdcfa-eval
((rec f (λ (x) (f x)))
 0)
((rec f (λ (n) (if0 n 1 (* n (f (- n 1))))))
 5)
((rec f (λ (x) (if0 x 0 (if0 (f (- x 1)) 2 3))))
 (+ 1 0))]

@section[#:style 'unnumbered #:tag "s:cache:formalism"]{Formal soundness and termination}

In this pearl, we have focused on the code and its intuitions rather
than rigorously establishing the usual formal properties of our
abstract interpreter, but this is just a matter of presentation: the
interpreter is indeed proven sound and computable.  We have formalized
@;{HACK: the appendix is not in scribble, so this is hard-coded.}
this co-inductive caching algorithm in Appendix A,
where we prove both that it always terminates, and that it computes a
sound over-approximation of concrete evaluation. Here, we give a short
summary of our metatheory approach.

In formalising the soundness of this caching algorithm, we extend a
standard big-step evaluation semantics into a @emph{big-step
reachability} semantics, which characterizes all intermediate
configurations which are seen between the evaluation of a single
expression and its eventual result. These two
notions—@emph{evaluation} which relates expressions to fully evaluated
results, and @emph{reachability} which characterizes intermediate
configuration states—remain distinct throughout the formalism.

After specifying evaluation and reachability for concrete evaluation,
we develop a @emph{collecting} semantics which gives a precise
specification for any abstract interpreter, and an @emph{abstract}
semantics which partially specifies a sound, over-approximating
algorithm w.r.t. the collecting semantics.

The final step is to compute an oracle for the @emph{abstract
evaluation relation}, which maps individual configurations to
abstractions of the values they evaluate to. To construct this cache,
we @emph{mutually} compute the least-fixed point of both the
evaluation and reachability relations: based on what is evaluated,
discover new things which are reachable, and based on what is
reachable, discover new results of evaluation. The caching algorithm
developed in this section is a slightly more efficient strategy for
solving the mutual fixed-point, by taking a deep exploration of the
reachability relation (up-to seeing the same configuration twice)
rather than applying just a single rule of inference.

