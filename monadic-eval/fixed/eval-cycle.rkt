#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

;; DVH: Why is this using alloc-0cfa?  Not a sound concrete eval.
(define-values/invoke-unit/infer
  (link monad-cycle@ state@ alloc-0cfa@ δ@ ev!@ ev-cycle@))

(define (eval e)
  (mrun ((fix (ev-cycle ev!)) e)))
