#lang racket
(provide eval)
(require "../units/eval-unit.rkt"
         "../units/fix-unit.rkt"
	 "../units/ev-bang-unit.rkt"
         "../units/id-unit.rkt"        
	 "../units/env-box-unit.rkt"        
         "../units/ref-box-unit.rkt"
         "../units/err-unit.rkt"         
         "../units/delta-unit.rkt")

(define-values/invoke-unit/infer
  (link ev!@ δ@ eval@ fix@ env-box@ id@ ref-box@ err@))
