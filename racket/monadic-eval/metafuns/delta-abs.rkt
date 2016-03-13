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
