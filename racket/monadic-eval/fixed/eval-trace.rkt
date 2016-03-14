#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link ListO@ monad-output@ state@ alloc-bang@ δ@ ev-trace@ ev!@))

(define (eval e) (mrun ((fix (ev-trace ev!)) e)))
