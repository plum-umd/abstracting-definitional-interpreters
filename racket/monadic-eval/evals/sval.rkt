#lang racket
(provide eval (struct-out both-ans))
(require "../units/sval-unit.rkt"
         "../units/eval-sto-unit.rkt"
         "../units/ev-symbolic-unit.rkt"
          "../units/delta-unit.rkt"
         "../units/err-unit.rkt"
         "../units/env-sto-unit.rkt"
	 "../both.rkt")

(define-values/invoke-unit/infer
  (link sval@ eval-sto@ env-sto@ ev-symbolic@ symbolic-δ@ err@))
