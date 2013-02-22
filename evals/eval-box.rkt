#lang racket
(provide eval)
(require "../units/ev-unit.rkt"
	 "../units/eval-pure-unit.rkt"
	 "../units/env-box-unit.rkt"
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-pure@ ev@ δ@ env-box@))
