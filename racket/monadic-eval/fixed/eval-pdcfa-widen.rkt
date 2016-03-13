#lang racket
(require "../fix.rkt"
         "../units.rkt"
         "../syntax.rkt"
         "../tests/tests.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-pdcfa-widen@ state-nd@ alloc-0cfa@ δ-abs@ ev!@ ev-cache@ eval-coind@))

(define (eval e)
  (mrun ((eval-coind (fix (ev-cache ev!))) e)))
