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

(define (number*? n) (or (eq? 'N n) (number? n)))

(define (crush v vs)
  (if (number*? v)
      (set-add (for/set ([v* vs] #:when (not (number*? v*))) v*) 'N)
      (set-add vs v)))

(define (ext a v)
  (update-store
   (λ (σ)
     (if (a . ∈ . σ)
         (σ a (crush v (σ a)))
         (σ a {set v})))))
