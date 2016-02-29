#lang racket
(provide eval)
(require "../units/ev-bang-unit.rkt"
         "../units/fix-unit.rkt"
         "../units/eval-sto-unit.rkt"	 
         "../units/sto-monad-unit.rkt"
         "../units/env-sto-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/ref-explicit-unit.rkt"
          "../units/err-unit.rkt")

(define-values/invoke-unit/infer
  (link ev!@ δ@ eval-sto@ fix@ env-sto@ sto-monad@ ref-explicit@ err@))
