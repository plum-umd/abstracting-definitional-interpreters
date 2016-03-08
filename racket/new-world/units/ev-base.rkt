#lang racket/unit
(require racket/match
         "../../monad-transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import alloc^ monad^ ref^ state^ δ^)
(export ev^)

(define ((ev ev) e)
  (with-monad M
    (match e

      [(vbl x)               (find x)]
      [(num n)               (return n)]

      [(ifz e₀ e₁ e₂)        (do v ← (ev e₀)
                                 b ← (truish? v)
                               (ev (if b e₁ e₂)))]

      [(op1 o e₀)            (do v ← (ev e₀)
                               (δ o v))]
      [(op2 o e₀ e₁)         (do v₀ ← (ev e₀)
                                 v₁ ← (ev e₁)
                                 (δ o v₀ v₁))]

      [(lrc f (lam x e₀) e₁) (rext f (lam x e₀) (ev e₁))]
      [(lam x e₀)            (do ρ ← ask
                               (return (cons (lam x e₀) ρ)))]
      [(app e₀ e₁)           (do (cons (lam x e₂) ρ) ← (ev e₀)
                                 v₁                  ← (ev e₁)
                               (local ρ (ext x v₁ (ev e₂))))]

      [(ref e₀)              (bind (ev e₀) mkbox)]
      [(drf e₀)              (bind (ev e₀) ubox)]
      [(srf e₀ e₁)           (do v₀ ← (ev e₀)
                                 v₁ ← (ev e₁)
                               (sbox v₀ v₁))]
      
      [_                     fail])))
