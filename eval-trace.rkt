#lang racket
(provide eval)
(require "eval-trace-unit.rkt"
	 "ev-bang-unit.rkt"
         "delta-unit.rkt"
         "env-sto-unit.rkt"
         "sto-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-trace@ ev!@ δ@ env-sto@ sto@))
