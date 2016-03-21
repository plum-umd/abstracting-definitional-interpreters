#lang racket
(require racket/match
         "../transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")
(provide ev-lazy!@)

(define-unit ev-lazy!@
  (import monad^ menv^ state^ force^ δ^ alloc^)
  (export ev-lazy!^)
  (init-depend monad^)

  (define-monad M)

  (define (((ev-lazy! ev0) ev) e)
    (match e
      [(app e₀ e₁) (do (cons (lam x e₂) ρ) ← (bind (ev e₀) force)
                     v₁ ← (ev e₁)
                     a  ← (alloc x)
                     (ext a v₁)
                     (local-env (ρ x a) (ev e₂)))]
      
      [(srf e₀ e₁) (do (cons 'box a) ← (bind (ev e₀) force)
                     v₁ ← (ev e₁)
                     (ext a v₁)
                     (return (cons 'box a)))]

      [_ ((ev0 ev) e)])))
