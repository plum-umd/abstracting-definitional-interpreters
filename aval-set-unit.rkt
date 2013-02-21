#lang racket/unit
(require racket/match
	 racket/set
	 "signatures.rkt"
	 "ev-bang-unit.rkt"
         "delta-unit.rkt"
         "sto-0cfa-unit.rkt")

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
  (unit-ans 'fail s))

