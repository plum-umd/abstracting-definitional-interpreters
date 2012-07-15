#lang racket
(require "ev-monad-sig.rkt")

(provide delta@ abs-delta@)

(define-unit delta@
  (import return^)
  (export δ^)
  (define (δ o . vs)
    (return (match* (o vs)
              [('add1 (list n))  (add1 n)]
              [('+ (list n1 n2)) (+ n1 n2)]))))

(define-unit abs-delta@
  (import return^)
  (export δ^)
  (define (δ o . vs)
    (return (match* (o vs)
              [('add1 (list n))  'N]
              [('+ (list n1 n2)) 'N]))))


(define-unit identity@
  (import)
  (export return^)
  (define (return x) x))
