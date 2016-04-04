#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-pdcfa@ state-nd@ alloc-0cfa@ δ-abs@ ev-ap@ ev-ap-cache@ eval-coind@))

(define (eval e)
  (mrun ((eval-coind (fix2 (ev-cache ev) ap)) e)))
