#lang racket
(provide (all-defined-out))
(define-signature ev-monad^
  (bind
   rec
   fail))
(define-signature sto-monad^
  (lookup-env
   alloc
   ralloc
   new
   sbox
   ubox))

