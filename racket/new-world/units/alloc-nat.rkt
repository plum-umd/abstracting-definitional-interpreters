#lang racket/unit
(require "../signatures.rkt")
(import)
(export alloc^)

(define alloc
  (let ([n (box 0)])
    (lambda (_)
      (let ([n* (unbox n)])
        (set-box! n (add1 n*))
        n*))))
