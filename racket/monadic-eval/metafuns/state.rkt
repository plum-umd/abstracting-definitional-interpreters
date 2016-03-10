#lang racket/unit

(require "../signatures.rkt"
         "../transformers.rkt")

(import monad^ menv^ mstore^)
(export state^)

(define-monad M)

(define (find x)
  (do ρ ← ask-env
      σ ← get-store
      (return (σ (ρ x)))))

(define (ext x a v m)
  (do ρ  ← ask-env
      ρ* ≔ (ρ x a)
      (update-store (λ (σ) (σ a v)))
      (local-env ρ* m)))
