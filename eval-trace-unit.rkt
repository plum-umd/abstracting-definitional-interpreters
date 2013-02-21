#lang racket/unit
(require racket/match
	 "signatures.rkt")

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
     (((f v) s) t)]))
