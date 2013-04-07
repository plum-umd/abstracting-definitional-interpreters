#lang racket
(provide eval)
(require "../units/eval-explicit-unit.rkt"
	 "../units/ev-bang-unit.rkt"
         "../units/sto-explicit-unit.rkt"
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-explicit@ ev!@ δ@ sto-explicit@))
