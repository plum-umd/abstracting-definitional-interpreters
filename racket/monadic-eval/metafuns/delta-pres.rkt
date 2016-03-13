#lang racket
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")
(provide δ-pres@)

(define-unit δ-pres@
  (import monad^)
  (export δ^)

  (define (δ . ovs)
    (with-monad M
      (match ovs
        [`(add1 ,(? number? n))  (return (add1 n))]
        [`(sub1 ,(? number? n))  (return (sub1 n))]
        [`(-    ,(? number? n))  (return (- n))]
        [`(+    ,(? number? n₁)
                ,(? number? n₂)) (return (+ n₁ n₂))]
        [`(-    ,(? number? n₁)
                ,(? number? n₂)) (return (- n₁ n₂))]
        [`(*    ,(? number? n₁)
                ,(? number? n₂)) (return (* n₁ n₂))]
        [`(quotient ,(? number? n₁) ,(? number? n₂))
         (if (zero? n₂)
             fail
             (return (quotient n₁ n₂)))]
        [`(add1 ,_)   (return 'N)]
        [`(sub1 ,_)   (return 'N)]
        [`(+ ,_ ,_) (return 'N)]
        [`(- ,_ ,_) (return 'N)]
        [`(* ,_ ,_) (return 'N)]
        [`(quotient ,_ ,(? number? n₂))
         (if (zero? n₂) fail (return 'N))]
        [`(quotient ,_ ,_)
         (mplus (return 'N) fail)])))

  (define (truish? v)
    (with-monad  M
      (match v
        ['N (mplus (return #t) (return #f))]
        [_  (return (zero? v))]))))
