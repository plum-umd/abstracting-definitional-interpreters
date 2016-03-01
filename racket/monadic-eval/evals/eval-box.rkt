#lang racket
(provide eval)
(require "../fix.rkt"
         "../units/ev-unit.rkt"
         "../units/env-box-unit.rkt"
	 "../units/err-unit.rkt"
         "../units/id-unit.rkt"
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link ev@ δ@ env-box@ id@ err@))

(define (eval e)
  (mrun ((fix ev) e (hash))))
