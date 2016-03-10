#lang racket/unit

(require "../signatures.rkt"
         "../transformers.rkt")

(import monad^ menv^ mstore^)
(export state^)

(define-monad M)

(define (find a)
  (do σ ← get-store
      (return (σ a))))

(define (ext a v) (update-store (λ (σ) (σ a v))))
