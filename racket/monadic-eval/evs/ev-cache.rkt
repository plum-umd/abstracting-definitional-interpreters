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

(define (((ev-cache ev₀) ev) e)
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
              v  ← ((ev₀ ev) e)
              σ  ← get-store
              (update-$ (λ (Σ) (Σ ς (set-add (Σ ς) (cons v σ)))))
              (return v)))))
