#lang racket/unit
(require racket/match
         "../signatures.rkt")

(import ev^)
(export unit^ bind^ rec^)

(define (rec e r) (ev e r))
(define ((unit v) s) (cons v s))
(define ((bind a f) s)
  (match (a s)
    [(cons v s) ((f v) s)]))
