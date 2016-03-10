#lang racket
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")
(provide δ@)

(define-unit δ@
  (import monad^)
  (export δ^)

  (define (δ . ovs)
    (with-monad M
      (match ovs
        [`(add1 ,n)   (return (add1 n))]
        [`(sub1 ,n)   (return (sub1 n))]
        [`(- ,n)      (return (- n))]
        [`(+ ,n1 ,n2) (return (+ n1 n2))]
        [`(- ,n1 ,n2) (return (- n1 n2))]
        [`(* ,n1 ,n2) (return (* n1 n2))]
        [`(quotient ,n1 ,n2)
         (if (zero? n2)
             fail
             (return (quotient n1 n2)))])))

  (define (truish? v)
    (with-monad M (return (zero? v)))))
