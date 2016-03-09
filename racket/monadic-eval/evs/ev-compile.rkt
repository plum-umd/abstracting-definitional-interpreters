#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^
        δ^ alloc^)
(export ev-compile^)

(define ((ev-compile ev) e)
  (with-monad M
    (match e

      [(vbl x)               (do ρ ← ask-env
                                 σ ← get-store
                                 (return (hash-ref σ (hash-ref ρ x))))]
      [(num n)               (return n)]
      [(ifz e₀ e₁ e₂)        (let ([c₀ (ev e₀)]
                                   [c₁ (ev e₁)]
                                   [c₂ (ev e₂)])
                               (do v ← c₀
                                   b ← (truish? v)
                                   (if b c₁ c₂)))]
      [(op1 o e₀)            (let ([c₀ (ev e₀)])
                               (do v ← c₀
                                   (δ o v)))]
      [(op2 o e₀ e₁)         (let ([c₀ (ev e₀)]
                                   [c₁ (ev e₁)])
                               (do v₀ ← c₀
                                   v₁ ← c₁
                                   (δ o v₀ v₁)))]
      [(lrc f (lam x e₀) e₁) (let ([c₀ (ev e₀)]
                                   [c₁ (ev e₁)])
                               (do ρ ← ask-env
                                   a ← (alloc f)
                                   ρ* ≔ (hash-set ρ f a)
                                   ; TODO: this needs to be hash-union for abstract stores
                                   (update-store (λ (σ) (hash-set σ a (list 'clo x c₀ ρ*))))
                                   (local-env ρ* c₁)))]
      [(lam x e₀)            (let ([c₀ (ev e₀)])
                               (do ρ ← ask-env
                                   (return (list 'clo x c₀ ρ))))]
      [(app e₀ e₁)           (let ([c₀ (ev e₀)]
                                   [c₁ (ev e₁)])
                               (do v₀ ← c₀
                                   (match v₀
                                     [(list 'clo x c₂ ρ)  (do v₁ ← c₁
                                                               a ← (alloc x)
                                                               ρ* ≔ (hash-set ρ x a)
                                                               (update-store (λ (σ) (hash-set σ a v₁)))
                                                               (local-env ρ* c₂))]
                                     [_                    fail])))]
      [(ref e₀)              (let ([c₀ (ev e₀)])
                               (do v₀ ← c₀
                                   a ← (alloc 'box)
                                   (update-store (λ (σ) (hash-set σ a v₀)))
                                   (return (list 'box a))))]
      [(drf e₀)              (let ([c₀ (ev e₀)])
                               (do v₀ ← c₀
                                   (match v₀
                                     [(list 'box a)  (do σ ← get-store
                                                         (return (hash-ref σ a)))]
                                     [_              fail])))]
      [(srf e₀ e₁)           (let ([c₀ (ev e₀)]
                                   [c₁ (ev e₁)])
                               (do v₀ ← c₀
                                   (match v₀
                                     [(list 'box a)  (do v₁ ← c₁
                                                         (update-store (λ (σ) (hash-set σ a v₁)))
                                                         (return (list 'box a)))]
                                     [_              fail])))]
      ['err                  fail])))
