#lang racket
(provide eval eval@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
	 "unit-sig.rkt"
         "bind-sig.rkt"
         "ev-unit.rkt"
	 "env-unit.rkt"
         "delta-unit.rkt")

(define-unit eval@
  (import ev^)
  (export eval^ unit^ bind^)
  (define (eval e) (ev e (hash)))
  (define (rec e r) (ev e r))
  (define (unit v) v)
  (define (bind v f) (f v)))

(define-values/invoke-unit/infer
  (link eval@ ev@ δ@ env@))
