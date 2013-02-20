#lang racket
(provide eval aval-set@)
(require "ev-sig.rkt"
	 "unit-sig.rkt"
         "eval-sig.rkt"
         "fail-sig.rkt"
         "bind-sig.rkt"
         "ev-bang-unit.rkt"
	 "delta-sig.rkt"
         "delta-unit.rkt"
         "sto-0cfa-unit.rkt")

;; Bounded store abstract set interpreter

;; eval : E -> [Setof Ans]

(define-unit aval-set@
  (import ev^)
  (export eval^ unit^ bind^ fail^ unit-ans^ unit-vals^)

  (define (eval e)
    ((ev e (hash)) (hash)))

  (define (rec e r) (ev e r))

  (define ((unit v) s)
    (unit-ans v s))

  (define ((bind a f) s)
    (for*/fold ([rs (set)])
      ([ret (a s)])
      (set-union rs
                 (match ret
                   [(cons 'fail s) (set (cons 'fail s))]
                   [(cons v s)
                    ((f v) s)]))))

  (define ((unit-vals vs) s)
    (unit-anss (for/set ([v vs])
                          (cons v s))))

  (define (unit-ans v s)
    (unit-anss (set (cons v s))))

  (define (unit-anss anss) anss)

  (define ((fail) s)
    (unit-ans 'fail s)))

(define-values/invoke-unit/infer
  (link aval-set@ ev!@ δ@ sto-0cfa@))
