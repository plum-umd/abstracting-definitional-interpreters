#lang racket
(provide eval)
(require "../fix.rkt"
         "../units/ev-unit.rkt"
         "../units/env-unit.rkt"
	 "../units/err-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/id-unit.rkt")

(define-values/invoke-unit/infer
  (link ev@ δ@ env@ id@ err@))

(define (eval e)
  (mrun ((fix ev) e (hash))))
