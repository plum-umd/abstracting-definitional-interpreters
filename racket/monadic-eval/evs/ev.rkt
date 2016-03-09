#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^
        δ^ alloc^)
(export ev^)

(define ((ev ev) e)
  (with-monad M
    (match e

      [(vbl x)               (do ρ ← ask-env
                                 σ ← get-store
                                 (return (hash-ref σ (hash-ref ρ x))))]
      [(num n)               (return n)]
      [(ifz e₀ e₁ e₂)        (do v ← (ev e₀)
                                 b ← (truish? v)
                                 (ev (if b e₁ e₂)))]
      [(op1 o e0)            (do v ← (ev e0)
                                 (δ o v))]
      [(op2 o e₀ e₁)         (do v₀ ← (ev e₀)
                                 v₁ ← (ev e₁)
                                 (δ o v₀ v₁))]
      [(lrc f (lam x e₀) e₁) (do ρ ← ask-env
                                 a ← (alloc f)
                                 ρ* ≔ (hash-set ρ f a)
                                 (update-store (λ (σ) (hash-set σ a (cons (lam x e₀) ρ*))))
                                 (local-env ρ* (ev e₁)))]
      [(lam x e₀)            (do ρ ← ask-env
                                 (return (cons (lam x e₀) ρ)))]
      [(app e₀ e₁)           (do v₀ ← (ev e₀)
                                 (match v₀
                                   [(cons (lam x e₂) ρ)
                                    (do v₁ ← (ev e₁)
                                        a  ← (alloc x)
                                        ρ* ≔ (hash-set ρ x a)
                                        (update-store (λ (σ) (hash-set σ a v₁)))
                                        (local-env ρ* (ev e₂)))]
                                   [_ fail]))]
      [(ref e₀)              (do v ← (ev e₀)
                                 a ← (alloc 'box)
                                 (update-store (λ (σ) (hash-set σ a v)))
                                 (return (cons 'box a)))]
      [(drf e₀)              (do v₀ ← (ev e₀)
                                 (match v₀
                                   [(cons 'box a)  (do σ ← get-store
                                                       (return (hash-ref σ a)))]
                                   [_              fail]))]
      [(srf e₀ e₁)           (do v₀ ← (ev e₀)
                                 (match v₀
                                   [(cons 'box a)  (do v₁ ← (ev e₁)
                                                       (update-store (λ (σ) (hash-set σ a v₁)))
                                                       (return (cons 'box a)))]
                                   [_              fail]))]
      ['err                  fail])))
