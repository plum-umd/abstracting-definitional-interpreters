#lang racket
(provide eval)
(require "../units/ev-unit.rkt"
         "../fix.rkt"        
	 "../units/sto-monad-unit.rkt"
	 "../units/env-sto-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/err-unit.rkt")

(define-values/invoke-unit/infer
  (link ev@ δ@ env-sto@ sto-monad@ err@))

(define (eval e)
  (mrun ((fix ev) e (hash))))
