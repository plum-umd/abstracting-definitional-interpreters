#lang racket
(provide eval)
(require "../units/ev-unit.rkt"
	 "../units/eval-pure-explicit-unit.rkt"
	 "../units/sto-monad-unit.rkt"
	 "../units/env-sto-unit.rkt"
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-pure-explicit@ ev@ sto-monad@ δ@ env-sto@))
