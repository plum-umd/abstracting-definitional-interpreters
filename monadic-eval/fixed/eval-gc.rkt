#lang racket
(require "../fix.rkt"
         "../units.rkt"
         "../syntax.rkt"
         "../tests/tests.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-gc@ state@ alloc@ δ@ ev-gc@ ev-collect@))

(define (eval e) (mrun ((fix (ev-collect ev)) e)))
