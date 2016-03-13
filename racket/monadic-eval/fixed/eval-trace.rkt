#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link ev-trace@ monad-trace@ state@ alloc@ δ@ ev!@))

(define (eval e) (mrun ((fix (ev-trace ev!)) e)))
