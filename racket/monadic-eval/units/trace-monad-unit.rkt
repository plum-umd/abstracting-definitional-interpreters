#lang racket/unit
(require racket/match
         "../signatures.rkt")
(import)
(export unit^ bind^ run^)

(define (mrun M) ((M (hash)) '()))

(define (((unit v) s) t) (cons (cons v s) t))
(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t) (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
