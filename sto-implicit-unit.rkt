#lang racket/unit
(require "ev-monad-sig.rkt")
(import return^)
(export sto-monad^)

(define (lookup-env r x)
  (return (hash-ref r x)))

(define (alloc f v)
  (return v))

(define (new v)
  (return (box v)))

(define (sbox a v)
  (set-box! a v)
  (return a))

(define (ubox a)
  (return (unbox a)))