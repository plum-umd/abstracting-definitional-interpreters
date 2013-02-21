#lang racket
(provide eval (struct-out both-ans))
(require "sval-unit.rkt"
         "ev-symbolic-unit.rkt"
         "sto-explicit-unit.rkt"
         "delta-unit.rkt"
	 "both.rkt")

(define-values/invoke-unit/infer
  (link sval@ ev-symbolic@ symbolic-δ@ sto-explicit@))
