#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")
(import monad^ monoid^)
(export δ^)

(define (δ . ovs)
  (with-monad  M
  (with-monoid O
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
       (oplus (return 'N) fail)]))))

(define (truish? v)
  (with-monad  M
  (with-monoid O
    (match v
      ['N (oplus  (return #t) (return #f))]
      [_  (return (zero? v))]))))
