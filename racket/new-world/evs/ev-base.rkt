#lang racket/unit
(require racket/match
         "../../monad-transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import alloc^ δ^ monad^ menv^ state^ ref^)
(export ev^)

(define ((ev ev) e)
  (with-monad M
    (match e

      [(vbl x)               (find x)]
      [(num n)               (return n)]

      [(ifz e0 e1 e2)        (do v ← (ev e0)
                                 b ← (truish? v)
                               (ev (if b e1 e2)))]
      [(op1 o e0)            (do v ← (ev e0)
                               (δ o v))]
      [(op2 o e₀ e₁)         (do v₀ ← (ev e₀)
                                 v₁ ← (ev e₁)
                               (δ o v₀ v₁))]

      [(lrc f (lam x e₀) e₁) (rext f (lam x e₀) (ev e₁))]
      [(lam x e₀)            (do ρ ← ask-env
                               (return (cons (lam x e₀) ρ)))]
      [(app e₀ e₁)           (do (cons (lam x e₂) ρ) ← (ev e₀)
                                 v₁                  ← (ev e₁)
                               (local-env ρ (ext x v₁ (ev e₂))))]

      [(ref e₀)              (bind (ev e₀) mkbox)]
      [(drf e₀)              (bind (ev e₀) ubox)]
      [(srf e₀ e₁)           (do v₀ ← (ev e₀)
                                 v₁ ← (ev e₁)
                               (sbox v₀ v₁))]
      
      [_                     fail])))
