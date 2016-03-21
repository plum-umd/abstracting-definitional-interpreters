#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-gc-count@ state@ alloc-bang@ δ@
        ev!@ ev-echo@ ev-gc-count@ gc-count@))

(define (eval e) (mrun ((fix (ev-gc (ev-count ev!))) e)))
