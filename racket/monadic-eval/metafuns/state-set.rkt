#lang racket/unit

(require racket/set
         "../signatures.rkt"
         "../transformers.rkt")

(import monad^ menv^ mstore^)
(export state^)

(define-monad  M)

(define (find x)
  (do ρ  ← ask-env
      σ  ← get-store
      vs ≔ (σ (ρ x))
      (for/monad+ ([v vs]) (return v))))

(define (ext x a v m)
  (do ρ  ← ask-env
      ρ* ≔ (ρ x a)
      (update-store
        (λ (σ) (σ a (λ (vs) (set-add vs v)) {set})))))
