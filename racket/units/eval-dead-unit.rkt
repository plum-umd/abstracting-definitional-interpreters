#lang racket/unit
(require racket/match
         racket/set
	 "../subexp.rkt"
	 "../signatures.rkt")

;; Computes dead code

(import ev^)
(export eval^ unit^ bind^ rec^ err^)

(define (eval e)
  (match (((rec e (hash)) (hash)) (subexps e))
    [(cons v dead)
     (cons v (set->list dead))]))

(define (((rec e r) s) t)
  (((ev e r) s) (set-remove t e)))

(define (((unit v) s) t) (cons (cons v s) t))
(define (((err) s) t) (cons (cons 'err s) t))
(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t) (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
