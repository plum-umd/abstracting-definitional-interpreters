#lang racket/unit
(require "signatures.rkt")
(import ev^)
(export unit^ bind^ rec^)

(define (rec e r) (ev e r))
(define (unit v) v)
(define (bind v f) (f v))
