#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-gc-count@ alloc-bang@ lazy-gc-state@ gc-count@ force-δ@
        ev-gc-count@ ev-lazy!@ eval-lazy!@ ev!@))

(define (eval e)
  (mrun ((eval-lazy! (fix (ev-gc (ev-count (ev-lazy! ev!))))) e)))
