#lang racket
(provide eval)
(require "../../monad-transformers.rkt"
         "../fix.rkt"
         "../signatures.rkt"
         "../units/ev-base.rkt"
         "../units/oev-trace.rkt"
         "../units/alloc-nat.rkt"
         "../units/delta-con.rkt"
         "../units/ref-explicit.rkt"
         "../units/st-explicit.rkt"
         "../monad/trace.rkt")

(define-values/invoke-unit/infer
  (link alloc-nat@
        trace@
        delta-con@
        ev-base@
        oev-trace@
        ref-explicit@
        st-explicit@))

;; eval : e → (cons v (cons σ τ))
(define (eval e)
  (mrun ((fix (oev ev)) e)))
