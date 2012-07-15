#lang racket
(provide return^ δ^ ev-monad^)
(define-signature return^ (return))
(define-signature δ^ (δ))
(define-signature ev-monad^
  (rec
   bind
   lookup-env
   fail
   alloc
   new
   sbox
   ubox))

