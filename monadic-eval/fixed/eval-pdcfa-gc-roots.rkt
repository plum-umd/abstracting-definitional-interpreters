#lang racket
(require "../fix.rkt"
         "../units.rkt")
(provide eval)

(define-values/invoke-unit/infer
  (link monad-pdcfa-gc@ state-nd@ alloc-0cfa@ δ-abs@ ev-gc@ ev-collect@ ev-cache-roots@ eval-coind-roots@))

(define (eval e)
  (mrun ((eval-coind (fix (ev-cache (ev-collect ev)))) e)))
