#lang racket
(provide (all-defined-out))

(define (lookup s r x)
  (hash-ref s (hash-ref r x)))

(define (update-sto s a v)
  (hash-set s a v))

(define (join-sto s a v)
  (hash-set s a (set-add (hash-ref s a (set)) v)))

(define (lookup-sto s a)
  (hash-ref s a))

(define (next s)
  (for/fold ([a 0])
            ([i (in-hash-keys s)])
    (max a (add1 i))))
