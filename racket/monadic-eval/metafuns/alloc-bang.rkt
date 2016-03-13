#lang racket/unit

(require "../signatures.rkt"
         "../transformers.rkt")

(import monad^)
(export alloc^)

(define n (box 0))

(define (alloc _)
  (with-monad M
    (let ([n* (unbox n)])
      (set-box! n (add1 n*))
      (return n*))))

