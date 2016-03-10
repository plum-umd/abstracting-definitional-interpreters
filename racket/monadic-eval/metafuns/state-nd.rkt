#lang racket/unit

(require racket/set
         "../map.rkt"
         "../signatures.rkt"
         "../transformers.rkt")

(import monad^ menv^ mstore^)
(export state^)

(define-monad M)

(define (find a)
  (do σ ← get-store
      (for/monad+ ([v (σ a)])
        (return v))))

(define (ext a v)
  (update-store
    (λ (σ) (σ a (if (a . ∈ . σ) (set-add (σ a) v) {set v})))))
