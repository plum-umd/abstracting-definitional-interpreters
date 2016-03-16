#lang racket/unit
(require racket/match
         racket/set
         "../gc.rkt"
         "../map.rkt"
         "../transformers.rkt"
         "../signatures.rkt")
(import monad^ mlive^ menv^ mstore^)
(export ev-gc^)
(init-depend monad^)

(define-monad M)

(define (((ev-gc ev0) ev) e)
  (do α  ← ask-live
      αₑ ≔ (count-fv e)
      v  ← (local-live (⊔ α αₑ) ((ev0 ev) e))
      ρ  ← ask-env
      σ  ← get-store
      (let* ([dead (for/set ([(x n) (∈ (⊔ α αₑ #:combine -))]
                             #:when (zero? n))
                     x)]
             [live (reachable (rng (map-remove ρ dead)) σ)])
        (do (update-store (λ (σ) (restrict σ live)))
            (return v)))))
