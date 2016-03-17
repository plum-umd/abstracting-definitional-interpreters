#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-dead@ ev-dead@ state@ eval-dead@ alloc-bang@ δ@ ev!@))

(define (eval e) (mrun ((eval-dead (fix (ev-dead ev!))) e)))
