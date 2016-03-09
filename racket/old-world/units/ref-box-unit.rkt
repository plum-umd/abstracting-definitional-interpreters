#lang racket/unit
(require "../signatures.rkt")

(import)
(export ref^)

(define new box)
(define ubox unbox)
(define (sbox a v)
  (set-box! a v)
  a)

