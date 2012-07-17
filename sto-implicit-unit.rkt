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
     (let* ([ph (make-placeholder #f)]
            [f (cons e (hash-set r x ph))])
       (placeholder-set! ph f)
       (return
        (make-reader-graph f)))]))

(define (new v)
  (return (box v)))

(define (sbox a v)
  (set-box! a v)
  (return a))

(define (ubox a)
  (return (unbox a)))