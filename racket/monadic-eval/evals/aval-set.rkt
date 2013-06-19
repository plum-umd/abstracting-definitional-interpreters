#lang racket
(provide eval)
(require "../units/aval-set-unit.rkt"
	 "../units/ev-bang-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/sto-0cfa-unit.rkt")

(define-values/invoke-unit/infer
  (link aval-set@ ev!@ δ@ sto-0cfa@))
