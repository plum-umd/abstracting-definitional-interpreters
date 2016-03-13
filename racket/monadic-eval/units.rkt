#lang racket/base

(define-syntax-rule (require-provide f ...)
  (begin
    (require f ...)
    (provide (all-from-out f ...))))

(require-provide
 ;; evals
 "evals/eval-dead.rkt"
 "evals/eval-coind.rkt"

 ;; open evs
 "evs/ev.rkt"
 "evs/ev-bang.rkt"

 ;; ext envs
 "evs/ev-ref.rkt"
 "evs/ev-dead.rkt"
 "evs/ev-reach.rkt"
 "evs/ev-symbolic.rkt"
 "evs/ev-trace.rkt"
 "evs/ev-cache.rkt"
 "evs/ev-compile.rkt"
 "evs/ev-debug.rkt"

 ;; monads
 "monad/monad.rkt"
 "monad/monad-dead.rkt"
 "monad/monad-nd.rkt"
 "monad/monad-pdcfa.rkt"
 "monad/monad-reach.rkt"
 "monad/monad-symbolic.rkt"
 "monad/monad-trace.rkt"

 ;; alloc
 "metafuns/alloc.rkt"
 "metafuns/alloc-bang.rkt"
 "metafuns/alloc-0cfa.rkt"

 ;; state
 "metafuns/state.rkt"
 "metafuns/state-nd.rkt"

 ;; δ
 "metafuns/delta.rkt"
 "metafuns/delta-pres.rkt"
 "metafuns/delta-symbolic.rkt"
 "metafuns/delta-abs.rkt"
 )
