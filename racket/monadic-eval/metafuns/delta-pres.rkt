#lang racket
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")
(provide δ-pres@)

(define-unit δ-pres@
  (import monad^ monoid^)
  (export δ^)

  (define (δ . ovs)
    (with-monad  M
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
        [`(,(or 'add1 'sub1 '+ '- '*) ,n) (return 'N)]
        [`(quotient ,n1 ,(? number? n2))
         (if (zero? n2) fail (return 'N))]
        [`(quotient ,n1 ,n2) 
         (mplus (return 'N) fail)])))

  (define (truish? v)
    (with-monad M
      (match v
        ['N (mplus  (return #t) (return #f))]
        [_  (return (zero? v))]))))
