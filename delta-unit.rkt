#lang racket
(require "delta-sig.rkt")
(require "unit-sig.rkt")

(provide delta@ abs-delta@ symbolic-delta@)

(define-unit delta@
  (import unit^)
  (export δ^)
  (define (δ o . vs)
    (unit
     (match* (o vs)
       [('add1 (list n))  (add1 n)]
       [('sub1 (list n))  (sub1 n)]
       [('- (list n))     (- n)]
       [('+ (list n1 n2)) (+ n1 n2)]
       [('- (list n1 n2)) (- n1 n2)]
       [('* (list n1 n2)) (* n1 n2)]))))

(define-unit abs-delta@
  (import unit^)
  (export δ^)
  (define (δ o . vs)
    (unit
     (match* (o vs)
       [('add1 (list n))  'N]
       [('sub1 (list n))  'N]
       [('+ (list n1 n2)) 'N]))))

(define-unit symbolic-delta@
  (import unit^)
  (export δ^)
  (define (δ o . vs)
    (unit
     (match* (o vs)
       [('add1 (list (? number? n)))
	(add1 n)]
       [('add1 (list s))
	`(add1 ,s)]
       [('+ (list (? number? n) (? number? m)))
	(+ n m)]
       [('+ (list s t))
	`(+ ,s ,t)]))))
