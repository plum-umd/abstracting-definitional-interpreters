#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

(define-signatures
  [eval^ : eval]
  [fix^  : fix]
  [ev^   : ev]
  [unit^ : unit]
  [bind^ : bind]
  [sto^  : ralloc alloc]
  [env^  : get ext]
  [δ^    : δ])

(define-signatures
  [err^ : err]
  [ref^ : new sbox ubox])

(define-signatures
  [symbolic^ : symbolic? both symbolic-apply])

(define-signatures
  [unit-ans^ : unit-ans]
  [unit-vals^ : unit-vals])
