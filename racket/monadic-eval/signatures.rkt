#lang racket
(provide (all-defined-out))

(define-syntax-rule
  (define-signatures [sig : name ...] ...)
  (begin (define-signature sig (name ...)) ...))

;; evs are one of:
;; - open ev : (exp → M value) → exp → M value
;; - ext  ev : open → open
;; an open is an interpreter that recurs through its first argument
;; an ext ev composes with an open to augment its behavior
;; the fixpoint of an open is a monadic interpreter : exp → M value
(define-signatures
  ;; open:
  [ev^          : ev]
  [ev!^         : ev!]

  ;; extensions:
  [ev-ref^      : ev-ref]
  [ev-trace^    : ev-trace]
  [ev-reach^    : ev-reach]
  [ev-dead^     : ev-dead]
  [ev-symbolic^ : ev-symbolic]
  [ev-compile^  : ev-compile]
  [ev-cache^    : ev-cache]
  [ev-debug^    : ev-debug])

;; these are open monadic interpreters, or evals, where
;;   eval : (exp → M value) → exp → M value
;; the distinction between an open ev and an eval is that an eval
;; expects to be given a fixed open ev, while an open ev expects
;; to be given its own fixpoint
(define-signatures
  [eval-dead^  : eval-dead]
  [eval-coind^ : eval-coind])

;; monad, monoid, and component-specific effects
(define-signatures
  [monad^   : M mrun mret]
  [monoid^  : O]

  ;; lifted effects for state-space components
  [mcache^    : ask-⊥ local-⊥ get-$ put-$ update-$]
  [mdead^     : get-dead put-dead update-dead]
  [menv^      : ask-env local-env]
  [mstore^    : get-store put-store update-store]
  [msymbolic^ : refine get-path-cond])

;; metafunctions
(define-signatures

  [alloc^    : alloc]
  ;; alloc : any → M addr
  ;;   allocate an address in the heap
  
  [state^    : find ext]
  ;; find : var → addr → M value
  ;;   finds the value bound to var @ addr in the heap
  ;; ext : var → addr → value → M unit
  ;;   bind var @ addr to value
  
  [δ^        : δ truish?]
  ;; δ : value ... → M value
  ;;   primitive operations on values
  ;; truish? : value → M bool
  ;;   does the value subsume 0?
  
  ;; 
  )
