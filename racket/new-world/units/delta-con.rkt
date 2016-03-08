#lang racket/unit
(require racket/match
         "../../monad-transformers.rkt"
         "../signatures.rkt")
(import monad^)
(export δ^)

(define (δ op . vs)
  (with-monad M
    (match* (op vs)
      [('add1 (list n))  (return (add1 n))]
      [('sub1 (list n))  (return (sub1 n))]
      [('- (list n))     (return (- n))]
      [('+ (list n1 n2)) (return (+ n1 n2))]
      [('- (list n1 n2)) (return (- n1 n2))]
      [('* (list n1 n2)) (return (* n1 n2))]
      [('quotient (list n1 n2))
       (if (zero? n2)
           (return 'err)
           (quotient n1 n2))])))

(define (truish? v)
  (with-monad M (return (zero? v))))
