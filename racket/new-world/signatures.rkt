#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

(define-signatures
  [ev^       : ev]
  [monoid^   : O]
  [monad^    : M mrun]
  [env^      : ask-env local-env]
  [store^    : get-store put-store]
  [trace^    : tell-trace hijack-trace]
  [cached^   : ask-gcache local-gcache get-lcache put-lcache]
  [state^    : rext ext find]
  [alloc^    : alloc]
  [ref^      : new sbox ubox]
  [δ^        : δ truish?]
  [symbolic^ : symbolic?])
