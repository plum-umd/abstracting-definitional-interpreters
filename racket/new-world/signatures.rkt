#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

;; evals
(define-signatures
  [eval-dead^  : eval-dead]
  [eval-pdcfa^ : eval-pdcfa])

;; evs
(define-signatures
  [ev^       : ev]
  [ev-trace^ : ev-trace]
  [ev-reach^ : ev-reach]
  [ev-dead^  : ev-dead])

;; monad, monoid, and component-specific effects
(define-signatures
  [monad^  : M mrun]
  [monoid^ : O]
  [mdead^  : get-dead put-dead]
  [menv^   : ask-env local-env]
  [mreach^ : tell-reach hijack-reach]
  [mstore^ : get-store put-store]
  [mtrace^ : tell-trace hijack-trace])

;; metafunctions
(define-signatures
  [alloc^ : alloc]
  [δ^     : δ truish?]
  [ref^   : mkbox sbox ubox]
  [state^ : rext ext find])
