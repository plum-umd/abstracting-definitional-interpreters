#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-pdcfa-widen-gc-count@ gc-count@ state-nd@ alloc-0cfa@ δ-abs@
        ev!@ ev-gc-count@ ev-cache@ eval-coind@))

(define (eval e)
  (mrun ((eval-coind (fix (ev-gc (ev-count (ev-cache ev!))))) e)))
