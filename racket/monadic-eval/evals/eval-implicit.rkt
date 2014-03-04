#lang racket
(provide eval)
(require "../units/eval-implicit-unit.rkt"
	 "../units/ev-bang-unit.rkt"
	 "../units/env-unit.rkt"
         "../units/sto-implicit-unit.rkt"
         "../units/err-unit.rkt"
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval!@ ev!@ δ@ env@ sto-implicit@))
