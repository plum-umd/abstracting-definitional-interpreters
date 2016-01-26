#lang racket
(provide eval)
(require "../units/trace-unit.rkt"
	 "../units/ev-bang-unit.rkt"
         "../units/trace-monad-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/env-sto-unit.rkt"
         "../units/sto-unit.rkt"
         "../units/err-unit.rkt")

(define-values/invoke-unit/infer
  (link ev!@ δ@ trace@ trace-monad@ sto@ err@))
