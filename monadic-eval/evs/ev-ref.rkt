#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt"
         "../syntax.rkt")

(import alloc^ monad^ menv^ mstore^ state^)
(export ev-ref^)

(define-monad M)

(define (((ev-ref ev0) ev) e)
  (match e
    [(ref e₀)    (do v ← (ev e₀)
                     a ← (alloc e)
                     (ext a v)
                     (return (cons 'box a)))]
    [(drf e₀)    (do (cons 'box a) ← (ev e₀)
                     v ← (find a)
                     (return v))]
    [(srf e₀ e₁) (do (cons 'box a) ← (ev e₀)
                     v₁ ← (ev e₁)
                     (ext a v₁)
                     (return (cons 'box a)))]
    
    [e ((ev0 ev) e)]))
