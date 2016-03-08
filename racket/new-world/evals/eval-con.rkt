#lang racket
(provide eval)
(require "../units/ev-base.rkt"
         "../units/delta-con.rkt"
         "../units/st-explicit.rkt"
         "../units/alloc-nat.rkt"
         "../monad/concrete.rkt"
         "../fix.rkt")

(define-values/invoke-unit/infer
  (link ev-base@
        delta-con@
        st-explicit@
        alloc-nat@
        concrete@))

;; evaluate : exp → (cons value store)
(define (eval e) (mrun ((fix ev) e)))
