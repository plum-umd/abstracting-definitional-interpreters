#lang racket
(provide eval eval@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "bind-sig.rkt"
         "fail-sig.rkt"
         "ev-bang-unit.rkt"
	 "unit-sig.rkt"
         "sto-explicit-unit.rkt"
         "delta-unit.rkt")

(define-unit eval@
  (import ev^)
  (export eval^ unit^ bind^ fail^)
  (define (eval e) ((ev e (hash)) (hash)))
  (define (rec e r) (ev e r))
  (define ((unit v) s) (cons v s))
  (define ((fail) s) (cons 'fail s))
  (define ((bind a f) s)
    (match (a s)
      [(cons 'fail s) (cons 'fail s)]
      [(cons v s)     ((f v) s)])))

(define-values/invoke-unit/infer
  (link eval@ ev!@ δ@ sto-explicit@))
