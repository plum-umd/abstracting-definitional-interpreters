#lang scribble/acmart @acmlarge

@(require scriblib/figure 
          scribble/manual 
          scribble/eval
          "evals.rkt"
          "bib.rkt")

@title[#:tag "s:widening"]{Widening the Store}

In this section, we show how to recover the well-known technique of
store-widening in our formulation of a definitional abstract
interpreter.  This example demonstrates the ease of which we can
construct existing abstraction choices.

The abstract interpreter we've constructed so far uses a
store-per-program-state abstraction, which is precise but
prohibitively expensive. A common technique to combat this cost is to
use a global ``widened'' store@~cite{dvanhorn:might-phd
dvanhorn:Shivers:1991:CFA}, which over-approximates each individual
store in the current set-up. This change is achieved easily in the
monadic setup by re-ordering the monad stack, a technique due to
@citet{local:darais-oopsla2015}. Whereas before we had
@racket[monad-cache@] we instead swap the order of @racket[StateT] for
the store and @racket[NondetT]:
@racketblock[
(ReaderT (FailT (NondetT (StateT+ (ReaderT (StateT+ ID))))))
]
we get a store-widened variant of the abstract interpreter. Because
@racket[StateT] for the store appears underneath nondeterminism, it
will be automatically widened. We write @racket[StateT+] to signify
that the cell of state supports such widening.

@figure["f:pres-delta" "An Alternative Abstraction for Precise Primitives"]{
@filebox[@racket[precise-δ@]]{
@racketblock[
(define (δ o n₀ n₁)
  (match* (o n₀ n₁)
    [('+ (? num?) (? num?)) (return (+ n₀ n₁))]
    [('+ _        _       ) (return 'N)] ... ))
(define (zero? v)
  (match v
    ['N (mplus (return #t) (return #f))]
    [_  (return (= 0 v))]))
]}
@filebox[@racket[store-crush@]]{
@racketblock[
(define (find a)
  (do σ ← get-store
      (for/monad+ ([v (σ a)])
        (return v))))
(define (crush v vs)
  (if (closure? v)
      (set-add vs v)
      (set-add (set-filter closure? vs) 'N)))
(define (ext a v)
  (update-store (λ (σ) (if (∈ a σ)
                           (σ a (crush v (σ a)))
                           (σ a (set v))))))
]}}

