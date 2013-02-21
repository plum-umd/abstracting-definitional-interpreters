#lang racket
(provide eval)
(require "pdcfa-unit.rkt"
         "ev-symbolic-unit.rkt"
         "delta-unit.rkt"
         "sto-0cfa-unit.rkt")

(define-values/invoke-unit/infer
  (link pdcfa@ ev-symbolic@ abs-δ@ sto-0cfa@))
