#lang racket
(require "../fix.rkt"
         "../units.rkt"
         "../syntax.rkt"
         "../tests/tests.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad@ state@ alloc-bang@ δ@ ev@ ev-debug@))

(define (eval e) (mrun ((fix ev) e)))
