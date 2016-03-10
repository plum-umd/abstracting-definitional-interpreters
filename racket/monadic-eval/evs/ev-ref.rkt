#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt"
         "../syntax.rkt")

(import alloc^ monad^ menv^ mstore^)
(export ev-ref^)

(define-monad M)

(define (((ev-ref ev0) ev) e)
  (match e
    [(ref e₀)    (do v ← (ev e₀)
                   a ← (alloc 'box)
                   (update-store (λ (σ) (hash-set σ a v)))
                   (return (cons 'box a)))]
    [(drf e₀)    (do (cons 'box a) ← (ev e₀)
                   σ ← get-store
                   v ≔ (hash-ref σ a)
                   (return v))]
    [(srf e₀ e₁) (do (cons 'box a) ← (ev e₀)
                   v₁ ← (ev e₁)
                   (update-store (λ (σ) (hash-set σ a v₁)))
                   (return (cons 'box a)))]
    
    [e ((ev0 ev) e)]))
