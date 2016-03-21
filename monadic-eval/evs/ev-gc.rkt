#lang racket/unit
(require racket/match
         racket/set
         "../transformers.rkt"
         "../syntax.rkt"
         "../gc.rkt"
         "../map.rkt"
         "../signatures.rkt")

(import monad^ menv^ state^ δ^ alloc^ gc^)
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
     (do ψ ← ask-roots
         ρ ← ask-env
         v ← (local-roots
              (set-union ψ
                         (roots e₁ ρ)
                         (roots e₂ ρ))
              (ev e₀))
         b ← (truish? v)
         (ev (if b e₁ e₂)))]

    [(op1 o e0)
     (do v ← (ev e0)
         (δ o v))]
    
    [(op2 o e₀ e₁)
     (do ψ ← ask-roots
         ρ ← ask-env
         v₀ ← (local-roots
               (set-union ψ (roots e₁ ρ))
               (ev e₀))
         v₁ ← (local-roots
               (set-union ψ (roots-v v₀))
               (ev e₁))
         (δ o v₀ v₁))]

    [(lrc f e₀ e₁) 
     (do ρ  ← ask-env
         a  ← (alloc f)
         (ext a (cons e₀ (ρ f a)))
         (local-env (ρ f a) (ev e₁)))]

    [(and (lam x e₀) l)
     (do ρ ← ask-env
       (return (cons l (restrict ρ (fv l)))))]

    [(app e₀ e₁)
     (do ψ ← ask-roots
         ρ ← ask-env
         v₀ ← (local-roots (set-union ψ (roots e₁ ρ)) (ev e₀))                                            
         v₁ ← (local-roots (set-union ψ (roots-v v₀)) (ev e₁))
         (cons (lam x e₂) ρ′) ≔ v₀
         a  ← (alloc x)
         (ext a v₁)
         (local-env (ρ′ x a) (ev e₂)))]

    ['err fail]))
