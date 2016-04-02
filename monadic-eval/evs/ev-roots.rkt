#lang racket/unit
(require racket/match
         racket/set
         "../transformers.rkt"
         "../syntax.rkt"
         "../gc.rkt"
         "../map.rkt"
         "../signatures.rkt")

(import monad^ menv^ state^ δ^ alloc^ gc^)
(export ev-roots^)
(init-depend monad^)

(define-monad M)

(define (((ev-roots ev_0) ev) e)
  (match e
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

    [(app e₀ e₁)
     (do ψ ← ask-roots
         ρ ← ask-env
         v₀ ← (local-roots (set-union ψ (roots e₁ ρ)) (ev e₀))
         v₁ ← (local-roots (set-union ψ (roots-v v₀)) (ev e₁))
         (cons (lam x e₂) ρ′) ≔ v₀
         a  ← (alloc x)
         (ext a v₁)
         (local-env (ρ′ x a) (ev e₂)))]

    [_ ((ev_0 ev) e)]))
