#lang racket/base

(provide eval)
(require racket/unit racket/match racket/set
         "../fix.rkt"
         "../signatures.rkt"
         "../syntax.rkt"
         "../evs/ev-symbolic.rkt"
         "../monad/symbolic.rkt"
         "../units/alloc-con.rkt"
         "../units/delta-symbolic.rkt")

(define-values/invoke-unit/infer
  (link symbolic@ alloc-con@ delta-symbolic@ ev-symbolic@))

(define (eval e)
  (mrun ((fix ev-symbolic) e)))
