#lang racket
(provide eval eval@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"
         "sto-implicit-unit.rkt"
         "delta-unit.rkt")

(define-unit eval@
  (import ev^ δ^ sto-monad^)
  (export eval^ return^ ev-monad^)
  (define (eval e) (ev e (hash)))
  (define (rec e r) (ev e r))
  (define (return v) v)
  (define (fail) 'fail)
  (define (bind a f)
    (match a
      ['fail 'fail]
      [v     (f v)])))

(define-values/invoke-unit/infer
  (link eval@ ev@ delta@ sto-implicit@))
