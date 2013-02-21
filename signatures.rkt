#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

(define-signatures
  [eval^ : eval]
  [ev^   : ev]
  [unit^ : unit]
  [bind^ : bind]
  [rec^  : rec]
  [env^  : get alloc ralloc]
  [δ^    : δ])

(define-signatures
  [err^ : err]
  [sto^ : new sbox ubox])

(define-signatures
  [symbolic-monad^ : symbolic? both symbolic-apply])

(define-signatures
  [unit-ans^ : unit-ans]
  [unit-vals^ : unit-vals])
