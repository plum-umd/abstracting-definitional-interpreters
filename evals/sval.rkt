#lang racket
(provide eval (struct-out both-ans))
(require "../units/sval-unit.rkt"
         "../units/ev-symbolic-unit.rkt"
         "../units/sto-explicit-unit.rkt"
         "../units/delta-unit.rkt"
	 "../both.rkt")

(define-values/invoke-unit/infer
  (link sval@ ev-symbolic@ symbolic-δ@ sto-explicit@))
