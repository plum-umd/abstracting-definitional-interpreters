#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-pdcfa-gc@ state-nd@ alloc-0cfa@ δ-abs@ ev@ ev-gc@ ev-cache@ eval-coind@))

(define (eval e)
  (mrun ((eval-coind (fix (ev-cache (ev-gc ev)))) e)))
