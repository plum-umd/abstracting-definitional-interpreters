#lang racket/unit
(require "signatures.rkt")

(import ev^)
(export eval^ unit^ bind^)

(define (eval e) (ev e (hash)))
(define (rec e r) (ev e r))
(define (unit v) v)
(define (bind v f) (f v))

