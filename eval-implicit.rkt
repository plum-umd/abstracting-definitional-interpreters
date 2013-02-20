#lang racket
(provide eval eval@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "bind-sig.rkt"
         "fail-sig.rkt"
         "ev-unit.rkt"
	 "env-unit.rkt"
	 "unit-sig.rkt"
         "sto-implicit-unit.rkt"
         "delta-unit.rkt")

(define-unit eval@
  (import ev^)
  (export eval^ unit^ bind^ fail^)
  (define (eval e) (ev e (hash)))
  (define (rec e r) (ev e r))
  (define (unit v) v)
  (define (fail) 'fail)
  (define (bind a f)
    (match a
      ['fail 'fail]
      [v     (f v)])))

(define-values/invoke-unit/infer
  (link eval@ ev@ δ@ env@ sto-implicit@))
