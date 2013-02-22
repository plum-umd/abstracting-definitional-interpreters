#lang racket/unit
(require racket/match
	 "signatures.rkt")

(import ev^)
(export eval^ unit^ bind^ rec^ err^)

(define (eval e) (((rec e (hash)) (hash)) '()))

(define (((rec e r) s) t)
  (((ev e r) s) (cons `(ev ,e ,r) t)))

(define (((unit v) s) t) (cons (cons v s) (cons `(unit ,v) t)))
(define (((err) s) t) (cons (cons 'err s) t))
(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t) (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
