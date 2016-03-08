#lang racket
(provide eval)
(require "../../monad-transformers.rkt"
         "../fix.rkt"
         "../signatures.rkt"
         "../units/ev-base.rkt"
         "../units/oev-reach.rkt"
         "../units/alloc-nat.rkt"
         "../units/delta-con.rkt"
         "../units/ref-explicit.rkt"
         "../units/st-explicit.rkt"
         "../monad/reach.rkt")

(define-values/invoke-unit/infer
  (link alloc-nat@
        reach@
        delta-con@
        ev-base@
        oev-reach@
        ref-explicit@
        st-explicit@))

;; eval : e → (cons (cons v σ) ℘(list v ρ σ))
(define (eval e)
  (mrun ((fix (oev ev)) e)))
