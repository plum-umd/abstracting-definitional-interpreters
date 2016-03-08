#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

(define-signatures
  [ev^       : ev]
  [oev^      : oev]
  [monoid^   : O]
  [monad^    : M mrun]
  [env^      : ask-env local-env]
  [store^    : get-store put-store]
  [dead^     : get-dead put-dead]
  [reach^    : tell-reach hijack-reach]
  [trace^    : tell-trace hijack-trace]
  [cached^   : ask-gcache local-gcache get-lcache put-lcache]
  [state^    : rext ext find]
  [ref^      : mkbox sbox ubox]
  [alloc^    : alloc]
  [δ^        : δ truish?]
  [symbolic^ : symbolic?])
