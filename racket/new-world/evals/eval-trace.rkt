#lang racket
(provide eval)
(require "../fix.rkt"
         "../signatures.rkt"
         "../../monad-transformers.rkt"
         "../monad/trace.rkt"
         "../units/ev-base.rkt"
         "../units/alloc-nat.rkt"
         "../units/delta-con.rkt"
         "../units/st-explicit.rkt")

(define-values/invoke-unit/infer
  (link alloc-nat@ ev-base@ delta-con@ st-explicit@ trace@))

(define ((ev-trace ev-trace) e)
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
        (tell-trace (list e ρ σ))
        ((ev ev-trace) e))))

(define (eval e)
  (mrun ((fix ev-trace) e)))
