#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link ev-loop@ state-crush@ δ-pres@ ev!@))

(define (eval e)
  (mrun ((fix (ev-loop ev!)) e)))
