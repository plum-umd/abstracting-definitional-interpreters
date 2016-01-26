#lang racket/unit
(require racket/match
         "../signatures.rkt")

(import)
(export unit^ bind^)

(define ((unit v) s)
  (cons v s))
(define ((bind a f) s)
  (match (a s)
    [(cons 'err s) (cons 'err s)]
    [(cons v s) ((f v) s)]))
