#lang racket
(require "../fix.rkt"
         "../syntax.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-nd@ state-nd@ alloc-bang@ δ@ ev!@))

;; eval : exp → ℘(value × σ)
(define (eval e) (mrun ((fix ev!) e)))
