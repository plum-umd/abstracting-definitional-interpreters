#lang racket
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")
(provide δ-abs@)

(define-unit δ-abs@
  (import monad^)
  (export δ^)

  (define (δ . ovs)
    (with-monad M
      (match ovs
        [`(add1 ,n)   (return 'N)]
        [`(sub1 ,n)   (return 'N)]
        [`(+ ,n₁ ,n₂) (return 'N)]
        [`(- ,n₁ ,n₂) (return 'N)]
        [`(* ,n₁ ,n₂) (return 'N)]
        [`(quotient ,n₁ ,(? number? n₂))
         (if (zero? n₂) fail (return 'N))]
        [`(quotient ,n₁ ,n₂) 
         (mplus (return 'N) fail)])))

  (define (truish? v)
    (with-monad  M
      (match v
        ['N (mplus (return #t) (return #f))]
        [_  (return (zero? v))]))))
