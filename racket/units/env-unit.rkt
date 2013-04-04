#lang racket/unit
(require racket/match
         "../signatures.rkt")

(import unit^)
(export env^)

(define (get r x)
  (unit (hash-ref r x)))

(define (alloc f v)
  (unit v))

(define (ralloc x v)
  (match v
    [(cons e r)
     (define p (make-placeholder #f))
     (define f (cons e (hash-set r x p)))
     (placeholder-set! p f)
     (unit (make-reader-graph f))]))
