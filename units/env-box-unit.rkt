#lang racket/unit
(require racket/match
	 "../signatures.rkt")

(import unit^)
(export env^)

(define (get r x)
  (unit (unbox (hash-ref r x))))
(define (alloc f v) (unit (box v)))
(define (ralloc x v)
  (match v
    [(cons e r)
     (define b (box #f))
     (define f (cons e (hash-set r x b)))
     (set-box! b f)
     (unit b)]))
