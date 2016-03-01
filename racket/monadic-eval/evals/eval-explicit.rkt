#lang racket
(provide eval)
(require "../fix.rkt"
         "../units/ev-bang-unit.rkt"
         "../units/sto-monad-unit.rkt"
         "../units/env-sto-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/ref-explicit-unit.rkt"
          "../units/err-unit.rkt")

(define-values/invoke-unit/infer
  (link ev!@ δ@ env-sto@ sto-monad@ ref-explicit@ err@))

(define (eval e)
  (mrun ((fix ev) e (hash))))
