#lang racket/unit
(require "../signatures.rkt")

(import unit^)
(export sto^)

(define (new v)
  (unit (box v)))

(define (sbox a v)
  (set-box! a v)
  (unit a))

(define (ubox a)
  (unit (unbox a)))
