#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-symbolic@ alloc@ state@ δ-symbolic@ ev-symbolic@ ev!@))

;; eval : exp -> ℘(((value ∪ (failure)) × σ) × φ)
(define (eval e)
  (mrun ((fix (ev-symbolic ev!)) e)))
