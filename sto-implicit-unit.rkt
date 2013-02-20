#lang racket/unit
(require (only-in racket shared match)
         "ev-monad-sig.rkt"
	 "env-sig.rkt"
	 "sto-sig.rkt"
	 "unit-sig.rkt")

(import unit^)
(export sto^ env^)

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
     (unit (make-reader-graph f))]))

(define (new v)
  (unit (box v)))

(define (sbox a v)
  (set-box! a v)
  (unit a))

(define (ubox a)
  (unit (unbox a)))