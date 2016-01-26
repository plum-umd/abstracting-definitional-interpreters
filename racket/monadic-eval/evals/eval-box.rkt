#lang racket
(provide eval)
(require "../units/ev-unit.rkt"
	 "../units/eval-unit.rkt"
	 "../units/env-box-unit.rkt"
	 "../units/err-unit.rkt"
         "../units/id-unit.rkt"
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link ev@ δ@ eval@ env-box@ id@ err@))
