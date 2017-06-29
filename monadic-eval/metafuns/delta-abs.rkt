#lang racket
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")
(provide δ-abs@)

(define-unit δ-abs@
  (import monad^)
  (export δ^)

  (define-monad M)

  (define (δ . ovs)
    (match ovs
      [(list 'add1 n)  (return 'N)]
      [(list 'sub1 n)  (return 'N)]
      [(list '+ n0 n1) (return 'N)]
      [(list '- n0 n1) (return 'N)]
      [(list '* n0 n1) (return 'N)]
      [(list '/ n0 (? number? n1))
       (if (= 0 n1) fail (return 'N))]
      [(list '/ n0 n1)
       (mplus (return 'N) fail)]))

  (define (truish? v)
    (match v
      ['N (mplus (return #t) (return #f))]
      [_  (return (zero? v))])))
