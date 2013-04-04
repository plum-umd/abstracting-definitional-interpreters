#lang racket
(require "../signatures.rkt")

(provide δ@ abs-δ@ pres-δ@ symbolic-δ@)

(define (δ/k sk fk o vs)
  (match* (o vs)
    [('add1 (list (? number? n)))  (sk (add1 n))]
    [('sub1 (list (? number? n)))  (sk (sub1 n))]
    [('- (list (? number? n)))     (sk (- n))]
    [('+ (list (? number? n1) (? number? n2))) (sk (+ n1 n2))]
    [('- (list (? number? n1) (? number? n2))) (sk (- n1 n2))]
    [('* (list (? number? n1) (? number? n2))) (sk (* n1 n2))]
    [(_ _) (fk o vs)]))

(define (abs-δ/k sk fk o vs)
  (match* (o vs)
    [('add1 (list n))  (sk 'N)]
    [('sub1 (list n))  (sk 'N)]
    [('+ (list n1 n2)) (sk 'N)]
    [('* (list n1 n2)) (sk 'N)]
    [(_ _) (fk o vs)]))

(define (δ-err o vs)
  (error "~a: undefined on ~a" o vs))

;; Concrete δ
(define-unit δ@
  (import unit^)
  (export δ^)
  
  (define (δ o . vs)
    (δ/k unit δ-err o vs)))
  

(define-unit abs-δ@
  (import unit^)
  (export δ^)
  (define (δ o . vs)
    (abs-δ/k unit δ-err o vs)))

;; Precision preserving abstract δ
(define-unit pres-δ@
  (import unit^)
  (export δ^)
  (define (δ o . vs)
    (δ/k unit
         (λ (o vs)
           (abs-δ/k unit δ-err o vs))
         o vs)))


(define-unit symbolic-δ@
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
