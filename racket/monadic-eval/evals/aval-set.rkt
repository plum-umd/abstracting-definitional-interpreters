#lang racket
(provide eval)
(require "../units/aval-set-unit.rkt"
	 "../units/ev-bang-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/sto-0cfa-unit.rkt"
         "../units/err-unit.rkt"
         "../units/eval-sto-unit.rkt")

(define-values/invoke-unit/infer
  (link ev!@ δ@ eval-sto@ aval-set@ sto-0cfa@ err@))
