#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link ev-compile@ monad@ state@ alloc@ δ@ ev!@))

(define (eval e) (mrun ((fix (ev-compile ev!)) e)))
