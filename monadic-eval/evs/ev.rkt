#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../syntax.rkt"
	 "../map.rkt"
         "../signatures.rkt")

(import monad^ menv^ state^ δ^ alloc^)
(export ev^)
(init-depend monad^)

(define-monad M)

(define ((ev ev) e)
  (match e
    [(vbl x)
     (do ρ ← ask-env
         (find (ρ x)))]
    
    [(num n) (return n)]
    
    [(ifz e₀ e₁ e₂)
     (do v ← (ev e₀)
         b ← (truish? v)
         (ev (if b e₁ e₂)))]

    [(op1 o e0)
     (do v ← (ev e0)
         (δ o v))]
    
    [(op2 o e₀ e₁)
     (do v₀ ← (ev e₀)
         v₁ ← (ev e₁)
         (δ o v₀ v₁))]

    [(lrc f e)
     (do ρ  ← ask-env
         a  ← (alloc f)
         ρ′ ≔ (ρ f a)
         v ← (local-env ρ′ (ev e))
	 (ext a v)
         (return v))]

    [(and (lam x e₀) l)
     (do ρ ← ask-env
       (return (cons l (restrict ρ (fv l)))))]

    [(app e₀ e₁)
     (do (cons (lam x e₂) ρ) ← (ev e₀)
         v₁ ← (ev e₁)
         a  ← (alloc x)        
         (ext a v₁)
         (local-env (ρ x a) (ev e₂)))]

    ['err fail]))
