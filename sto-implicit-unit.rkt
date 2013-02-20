#lang racket
(require "sto-sig.rkt"
	 "unit-sig.rkt")

(provide sto-implicit@)

(define-unit sto-implicit@
  (import unit^)
  (export sto^)

  (define (new v)
    (unit (box v)))

  (define (sbox a v)
    (set-box! a v)
    (unit a))

  (define (ubox a)
    (unit (unbox a))))
