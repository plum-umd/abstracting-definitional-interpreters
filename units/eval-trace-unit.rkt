#lang racket/unit
(require racket/match
         racket/list
	 "../signatures.rkt")

(import ev^)
(export eval^ unit^ bind^ rec^ err^)

(define (eval e) (((rec e (hash)) (hash)) empty))

(define (((rec e r) s) t)
  (((ev e r) s) (cons (list e r s) t)))

(define (((unit v) s) t) (cons (cons v s) t))
(define (((err) s) t) (cons (cons 'err s) t))
(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t) (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
