#lang racket
(provide eval)
(require "eval-explicit-unit.rkt"
	 "ev-bang-unit.rkt"
         "sto-explicit-unit.rkt"
         "delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-explicit@ ev!@ δ@ sto-explicit@))
