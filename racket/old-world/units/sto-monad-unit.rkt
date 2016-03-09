#lang racket/unit
(require racket/match
         "../signatures.rkt")

(import)
(export monad^)

(define (mrun M)
  (M (hash)))

(define (return v) 
  (λ (s)
    (cons v s)))
(define ((bind a f) s)
  (match (a s)
    [(cons 'err s) (cons 'err s)]
    [(cons v s) ((f v) s)]))
