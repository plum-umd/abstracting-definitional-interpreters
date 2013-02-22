#lang racket
(provide eval)
(require "../units/pdcfa-unit.rkt"
         "../units/ev-symbolic-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/sto-0cfa-unit.rkt")

(define-values/invoke-unit/infer
  (link pdcfa@ ev-symbolic@ abs-δ@ sto-0cfa@))
