#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad@ alloc-bang@ state@ δ@ ev!@))

(define (eval e) (mrun ((fix ev!) e)))
