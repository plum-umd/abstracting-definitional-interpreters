#lang racket/unit
(require "../../monad-transformers.rkt"
         "../signatures.rkt")
(import monad^ alloc^ env^ store^)
(export state^)

(define (find x)
  ;; find : var → M value
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
      (return (hash-ref σ (hash-ref ρ x))))))

(define (ext x v m)
  ;; ext : var → value → M a → M a
  ;; binds [x] to [v], returning the new env
  (with-monad M
    (do ρ  ← ask-env
        σ  ← get-store
        a  ← (alloc σ)
        ρ* ≔ (hash-set ρ x a)
        σ* ≔ (hash-set σ a v)
      (put-store σ*)
      (local-env ρ* m))))

(define (rext x e m)
  ;; rext : var → exp → M a → M a
  (with-monad M
    (do ρ  ← ask-env
        σ  ← get-store
        a  ← (alloc σ)
        ρ* ≔ (hash-set ρ x a)
        σ* ≔ (hash-set σ a (cons e ρ*))
      (put-store σ*)
      (local-env ρ* m))))
