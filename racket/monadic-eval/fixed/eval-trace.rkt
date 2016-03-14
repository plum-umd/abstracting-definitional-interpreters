#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-trace@ state@ alloc-bang@ δ@ ev-trace@ ev!@))

(define (eval e) (mrun ((fix (ev-trace ev!)) e)))
