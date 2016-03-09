#lang racket/unit

(require "../signatures.rkt")

(import)
(export alloc^)

(define n (box 0))

(define (alloc _)
  (let ([n* (unbox n)])
       (set-box! n (add1 n*))
       n*))

