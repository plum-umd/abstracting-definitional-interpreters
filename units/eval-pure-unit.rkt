#lang racket/unit
(require "../signatures.rkt")
(require "id-monad-unit.rkt")

(import ev^)
(export eval^ unit^ bind^ rec^)

(define (eval e) (rec e (hash)))
(define (rec e r) (ev e r))
(define (unit v) v)
(define (bind v f) (f v))
