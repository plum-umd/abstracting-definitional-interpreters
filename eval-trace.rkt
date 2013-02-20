#lang racket
(provide eval eval-trace@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "bind-sig.rkt"
         "fail-sig.rkt"
         "ev-bang-unit.rkt"
	 "unit-sig.rkt"
         "delta-unit.rkt"
         "sto-explicit-unit.rkt"
         "store.rkt"
         "syntax.rkt")

;; Trace evaluator

(define-unit eval-trace@
  (import ev^)
  (export eval^ unit^ bind^ fail^)

  (define (eval e)
    (define s (hash))
    (define r (hash))
    (((ev e r) s) (list `(ev ,e ,r))))

  (define (((rec e r) s) t)
    (((ev e r) s) (cons `(ev ,e ,r) t)))

  (define (((unit v) s) t) (cons (cons v s) (cons `(unit ,v) t)))
  (define (((fail) s) t) (cons (cons 'fail s) t))
  (define (((bind a f) s) t)
    (match ((a s) t)
      [(cons (cons 'fail s) t) (cons (cons 'fail s) t)]
      [(cons (cons v s) t)
       (((f v) s) t)])))


(define-values/invoke-unit/infer
  (link eval-trace@ ev!@ δ@ sto-explicit@))
