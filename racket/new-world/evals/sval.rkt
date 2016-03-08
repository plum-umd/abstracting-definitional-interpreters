#lang racket/base

(provide eval)
(require racket/unit racket/match racket/set
         "../fix.rkt"
         "../signatures.rkt"
         "../syntax.rkt"
         "../monad/symbolic.rkt"
         "../units/env.rkt"
         "../units/alloc-nat.rkt"
         "../units/delta-symbolic.rkt"
         "../units/st-explicit.rkt"
         "../units/ev-symbolic.rkt")

(define-values/invoke-unit/infer
  (link symbolic@ env@ ev-symbolic@ st-explicit@ alloc-nat@ delta-symbolic@))

(define (eval e)
  (mrun ((fix ev) e)))
