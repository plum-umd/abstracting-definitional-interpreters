#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

(define-signatures
  [eval^ : eval] ; go away
  [fix^  : fix]  ; go away
  [ev^   : ev]   ; go away
  [unit^ : unit] ; combine
  [bind^ : bind]
  [run^  : mrun]
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
