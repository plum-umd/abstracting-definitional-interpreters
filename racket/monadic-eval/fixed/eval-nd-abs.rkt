#lang racket
(require "../fix.rkt"
         "../syntax.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-nd@ state-crush@ alloc-0cfa@ δ-abs@ ev!@))

;; eval : exp → ℘(a × σ)
(define (eval e) (mrun ((fix ev!) e)))
