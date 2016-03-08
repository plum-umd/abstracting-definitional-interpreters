#lang racket/unit
(require "../signatures.rkt")
(import)
(export alloc^)

(define alloc
  (let ([n 0])
    (λ (_)
      (begin0 n
        (set! n (+ 1 n))))))
