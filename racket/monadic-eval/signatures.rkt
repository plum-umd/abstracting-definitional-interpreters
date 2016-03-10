#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

;; evals
(define-signatures
  [eval-dead^  : eval-dead]
  [eval-coind^ : eval-coind])

;; evs
(define-signatures
  [ev^          : ev]
  [ev-ref^      : ev-ref]
  [ev-trace^    : ev-trace]
  [ev-reach^    : ev-reach]
  [ev-dead^     : ev-dead]
  [ev-symbolic^ : ev-symbolic]
  [ev-compile^  : ev-compile]
  [ev-cache^    : ev-cache])

;; monad, monoid, and component-specific effects
(define-signatures
  [monad^   : M mrun]
  [monoid^  : O]
  [mcached^ : ask-⊥ local-⊥ get-$ put-$ update-$]
  [mdead^   : get-dead put-dead update-dead]
  [menv^    : ask-env local-env]
  [mreach^  : tell-reach hijack-reach]
  [mstore^  : get-store put-store update-store]
  [mtrace^  : tell-trace hijack-trace])

;; metafunctions
(define-signatures
  [alloc^    : alloc]
  [state^    : find ext]
  [δ^        : δ truish?]
  [symbolic^ : symbolic? refine get-path-cond])
