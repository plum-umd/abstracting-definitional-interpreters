#lang racket
(provide eval)
(require "../units/eval-set-unit.rkt"
	 "../units/ev-bang-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/sto-set-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-set@ ev!@ δ@ sto-set@))
