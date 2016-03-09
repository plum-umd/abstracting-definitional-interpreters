#lang racket/base

(define-syntax-rule (require-provide f ...)
  (begin
    (require f ...)
    (provide (all-from-out f ...))))

(require-provide
 ;; evals
 "evals/eval-dead.rkt"

 ;; evs
 "evs/ev.rkt"
 "evs/ev-base.rkt"
 "evs/ev-dead.rkt"
 "evs/ev-reach.rkt"
 "evs/ev-symbolic.rkt"
 "evs/ev-trace.rkt"
 "evs/ev-compile.rkt"

 ;; monads
 "monad/monad-con.rkt"
 "monad/monad-dead.rkt"
 "monad/monad-reach.rkt"
 "monad/monad-symbolic.rkt"
 "monad/monad-trace.rkt"

 ;; alloc
 "units/alloc-con.rkt"

 ;; delta
 "units/delta-con.rkt"
 "units/delta-pres.rkt"
 "units/delta-symbolic.rkt"
 "units/delta-top.rkt"
 )
