#lang racket/unit
(require racket/match
         racket/set
	 "../signatures.rkt")

(import ev^)
(export eval^ unit^ bind^ rec^ err^)

(define (eval e)
  (match (((rec e (hash)) (hash)) (set))
    [(cons v reach)
     (cons v (set->list reach))]))

(define (((rec e r) s) t)
  (((ev e r) s) (set-add t (list e r s))))

(define (((unit v) s) t) (cons (cons v s) t))
(define (((err) s) t) (cons (cons 'err s) t))
(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t) (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
