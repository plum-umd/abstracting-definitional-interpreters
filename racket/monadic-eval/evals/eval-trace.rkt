#lang racket
(provide eval)
(require "../units/trace-unit.rkt"
         "../units/eval-sto-trace-unit.rkt"
         "../units/fix-unit.rkt"
         "../units/ev-bang-unit.rkt"
         "../units/trace-monad-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/env-sto-unit.rkt"
         "../units/sto-unit.rkt"
         "../units/err-unit.rkt"
         "../signatures.rkt")

(define-compound-unit trace-ev@
  (import [U : unit^]
          [B : bind^]
          [D : δ^]
          [E : env^]
          [S : sto^]
          [R : ref^]
          [X : err^])
  (export EV)
  (link (((EV : ev^)) trace@ EV0)
        (((EV0 : ev^)) ev!@ U B D E S R X)))

(define-values/invoke-unit/infer
  (link trace-ev@ δ@ eval-sto-trace@ fix@ trace-monad@ sto@ err@))
