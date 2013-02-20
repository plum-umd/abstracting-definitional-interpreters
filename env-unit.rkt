#lang racket
(require "env-sig.rkt"
	 "unit-sig.rkt")

(provide env@)

(define-unit env@
  (import unit^)
  (export env^)

  (define (lookup-env r x)
    (unit (hash-ref r x)))

  (define (alloc f v)
    (unit v))

  (define (ralloc x v)
    (match v
      [(cons e r)
       (define p (make-placeholder #f))
       (define f (cons e (hash-set r x p)))
       (placeholder-set! p f)
       (unit (make-reader-graph f))])))
