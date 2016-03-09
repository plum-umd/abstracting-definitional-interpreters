#lang racket/base

(provide eval)
(require racket/unit racket/match racket/set
         "../fix.rkt"
         "../signatures.rkt"
         "../syntax.rkt"
         "../evs/ev-symbolic.rkt"
         "../monad/symbolic.rkt"
         "../units/alloc-nat.rkt"
         "../units/delta-symbolic.rkt"
         "../units/st-explicit.rkt"
         "../units/ref-explicit.rkt")

(define-values/invoke-unit/infer
  (link symbolic@ ev-symbolic@ st-explicit@ alloc-nat@ delta-symbolic@ ref-explicit@))

(define (eval e)
  (mrun ((fix ev-symbolic) e)))
