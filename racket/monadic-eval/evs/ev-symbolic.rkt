#lang racket/unit

(require racket/match
         "../signatures.rkt" "../syntax.rkt" "../transformers.rkt")

(import monad^ menv^ mstore^
        δ^ alloc^)
(export ev-symbolic^)

(define ((ev-symbolic ev) e)
  (with-monad M
    (match e
      [(vbl x)
       (do ρ ← ask-env
           σ ← get-store
           (return (hash-ref σ (hash-ref ρ x))))]
      [(num n)
       (return n)]
      [(sym x)
       (return x)]
      [(ifz e0 e1 e2)
       (do v ← (ev e0)
           n ← (δ 'flip v)
           (case n
             [(0) (ev e2)]
             [(1) (ev e1)]))]
      [(op1 o e0)
       (do v ← (ev e0)
           (δ o v))]
      [(op2 o e0 e1)
       (do v0 ← (ev e0)
           v1 ← (ev e1)
           (δ o v0 v1))]
      [(ref e)
       (do v ← (ev e)
           a ← (alloc 'box)
           (update-store (λ (σ) (hash-set σ a v)))
           (return (cons 'box a)))]
      [(drf e)
       (do v ← (ev e)
           (match v
             [(cons 'box a)  
              (do σ ← get-store
                  (return (hash-ref σ a)))]
             [_              fail]))]
      [(srf e₀ e₁)
       (do v₀ ← (ev e₀)
         (match v₀
           [(cons 'box a)  
            (do v₁ ← (ev e₁)
                (update-store (λ (σ) (hash-set σ a v₁)))
                (return (cons 'box a)))]
           [_              fail]))]
      [(lrc f (lam x e₀) e₁)
       (do ρ ← ask-env
           a ← (alloc f)
           ρ* ≔ (hash-set ρ f a)
           ; TODO: this needs to be hash-union for abstract stores
           (update-store (λ (σ) (hash-set σ a (cons (lam x e₀) ρ*))))
           (local-env ρ* (ev e₁)))]
      [(lam x e0)
       (do ρ ← ask-env
           (return (cons (lam x e0) ρ)))]
      [(app e₀ e₁)
       (do v₀ ← (ev e₀)
           (match v₀
             [(cons (lam x e₂) ρ)  
              (do v₁ ← (ev e₁)
                  a ← (alloc x)
                  ρ* ≔ (hash-set ρ x a)
                  (update-store (λ (σ) (hash-set σ a v₁)))
                  (local-env ρ* (ev e₂)))]
      ['err fail]))])))
