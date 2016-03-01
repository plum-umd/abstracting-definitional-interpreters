#lang racket
(provide eval (struct-out both-ans))
(require "../fix.rkt"
         "../units/sval-unit.rkt"
         "../units/ev-symbolic-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/err-unit.rkt"
         "../units/env-sto-unit.rkt"
	 "../both.rkt")

(define-values/invoke-unit/infer
  (link sval@ env-sto@ ev-symbolic@ symbolic-δ@ err@))

(define (eval e)
  (mrun ((fix ev) e (hash))))