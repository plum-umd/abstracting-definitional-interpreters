#lang racket/unit
(require "../signatures.rkt")

(import ev^)
(export eval^)
(define (eval e)
  (define (ev′ e ρ)  ((ev e ρ) ev′))
  (ev′ e (hash)))