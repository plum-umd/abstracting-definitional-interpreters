#lang racket
(provide eval)
(require "eval-trace-unit.rkt"
	 "ev-bang-unit.rkt"
         "delta-unit.rkt"
         "sto-explicit-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-trace@ ev!@ δ@ sto-explicit@))
