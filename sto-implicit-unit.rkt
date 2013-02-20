#lang racket/unit
(require (only-in racket shared match)
         "ev-monad-sig.rkt")
(import return^)
(export sto-monad^)

(define (lookup-env r x)
  (return (hash-ref r x)))

(define (alloc f v)
  (return v))

(define (ralloc x v)
  (match v
    [(cons e r)
     (define p (make-placeholder #f))
     (define f (cons e (hash-set r x p)))
     (placeholder-set! p f)
     (return (make-reader-graph f))]))

(define (new v)
  (return (box v)))

(define (sbox a v)
  (set-box! a v)
  (return a))

(define (ubox a)
  (return (unbox a)))