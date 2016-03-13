#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-reach@ state@ alloc@ δ@ ev!@ ev-reach@))

(define (eval e)
  (mrun ((fix (ev-reach ev!)) e)))
