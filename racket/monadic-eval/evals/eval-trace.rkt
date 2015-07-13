#lang racket
(provide eval)
(require "../units/trace-unit.rkt"
	 "../units/ev-bang-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/env-sto-unit.rkt"
         "../units/sto-unit.rkt")

(define-values/invoke-unit/infer
  (link trace@ ev!@ δ@ env-sto@ sto@))
