#lang scribble/manual
@(require scribble/eval
	  scriblib/figure
          "evals.rkt")

@title[#:tag "fixing-cache"]{Fixing the Cache}

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
a computable---and we conjecture, sound---abstraction of the original
definitional interpreter.  Note that the iterated evaluator always
terminates: the cache resulting from each run of the evaluator
contains @emph{at least} as much information as the prior cache, each
run of the evaluator terminates, so the iterated evaluator terminates
by the same principle as before: the cache monotonically grows and is
finite in size.

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
