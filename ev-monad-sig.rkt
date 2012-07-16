#lang racket
(provide (all-defined-out))
(define-signature return^ (return))
(define-signature return-vals^ (return-vals))
(define-signature return-ans^ (return-ans))
(define-signature δ^ (δ))
(define-signature ev-monad^
  (bind
   rec
   fail))
(define-signature sto-monad^
  (lookup-env
   alloc
   new
   sbox
   ubox))

