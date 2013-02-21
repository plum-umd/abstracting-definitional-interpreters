#lang racket
(provide eval)
(require "eval-implicit-unit.rkt"
	 "ev-bang-unit.rkt"
	 "env-unit.rkt"
         "sto-implicit-unit.rkt"
         "delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval!@ ev!@ δ@ env@ sto-implicit@))
