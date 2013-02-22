#lang racket
(provide eval)
(require "../units/ev-unit.rkt"
	 "../units/eval-pure-unit.rkt"
	 "../units/id-monad-unit.rkt"
	 "../units/env-unit.rkt"
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-pure@ ev@ id-monad@ δ@ env@))
