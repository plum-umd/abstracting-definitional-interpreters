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
          (for/monad+ ([v (Σ ς)]) (return v))
          (do Σ⊥ ← ask-⊥
              (put-$ (Σ ς (if (∈ ς Σ⊥) (Σ⊥ ς) (set))))
              v  ← ((ev₀ ev) e)
              (update-$ (λ (Σ) (Σ ς (set-add (Σ ς) v))))
              (return v)))))
