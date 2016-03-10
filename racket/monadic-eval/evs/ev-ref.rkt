#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt"
         "../syntax.rkt")

(import alloc^ monad^ menv^ mstore^)
(export ev-ref^)

(define (((ev-ref ev0) ev) e)
  (with-monad M
    (match e
      
      [(ref e₀)    (do v ← (ev e₀)
                       a ← (alloc 'box)
                       (update-store (λ (σ) (σ a v)))
                       (return (cons 'box a)))]
      [(drf e₀)    (do v₀ ← (ev e₀)
                       (match v₀
                         [(cons 'box a)
                          (do σ ← get-store
                              v ≔ (σ a)
                              (return v))]
                         [_ fail]))]
      [(srf e₀ e₁) (do v₀ ← (ev e₀)
                       (match v₀
                         [(cons 'box a)
                          (do v₁ ← (ev e₁)
                              (update-store (λ (σ) (σ a v₁)))
                              (return (cons 'box a)))]
                         [_ fail]))]
      ['err fail]
      [e ((ev0 ev) e)])))
