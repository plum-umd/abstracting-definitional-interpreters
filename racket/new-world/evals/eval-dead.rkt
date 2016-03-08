#lang racket
(provide eval)
(require "../../monad-transformers.rkt"
         "../fix.rkt"
         "../signatures.rkt"
         "../units/ev-base.rkt"
         "../units/oev-dead.rkt"
         "../units/alloc-nat.rkt"
         "../units/delta-con.rkt"
         "../units/ref-explicit.rkt"
         "../units/st-explicit.rkt"
         "../monad/dead.rkt")

(define-values/invoke-unit/infer
  (link alloc-nat@
        dead@
        delta-con@
        ev-base@
        oev-dead@
        ref-explicit@
        st-explicit@))

;; eval : e → (cons (cons v θ) σ)
(define (eval e)
  (mrun ((fix (oev ev)) e)))
