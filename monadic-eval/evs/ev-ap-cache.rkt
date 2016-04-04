#lang racket/unit
(require racket/match
         racket/set
         "../map.rkt"
         "../transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^ mcache^ δ^ alloc^)
(export ev-cache^)
(init-depend monad^)

(define-monad M)

;; this is exactly the same as defined ev-cache.rkt, but
;; uses arity consistent with eval-apply set-up.
(define (((ev-cache ev₀) ev ap) e)
  (do ρ ← ask-env
      σ ← get-store
      ς ≔ (list e ρ σ)
      Σ ← get-$
      (if (∈ ς Σ)
          (for/monad+ ([v.σ (Σ ς)])
            (do (put-store (cdr v.σ))
                (return (car v.σ))))
          (do Σ⊥ ← ask-⊥
              (put-$ (Σ ς (if (∈ ς Σ⊥) (Σ⊥ ς) (set))))
              v  ← ((ev₀ ev ap) e)
              σ  ← get-store
              (update-$ (λ (Σ) (Σ ς (set-add (Σ ς) (cons v σ)))))
              (return v)))))
