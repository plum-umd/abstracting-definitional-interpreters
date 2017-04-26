#lang racket
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")
(provide δ@)

(define-unit δ@
  (import monad^)
  (export δ^)
  (define-monad M)

  (define (δ . ovs)
    (match ovs
      [(list 'add1 n)  (return (add1 n))]
      [(list 'sub1 n)  (return (sub1 n))]
      [(list '- n)     (return (- n))]
      [(list '+ n1 n2) (return (+ n1 n2))]
      [(list '- n1 n2) (return (- n1 n2))]
      [(list '* n1 n2) (return (* n1 n2))]
      [(list 'quotient n1 n2)
       (if (zero? n2)
           fail
           (return (quotient n1 n2)))]
      [(list '¬ n) (return (if (= n 0) 0 1))]
      [_ fail]))

  (define (truish? v)
    (return (zero? v))))
