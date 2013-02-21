#lang racket
(provide eval)
(require "ev-unit.rkt"
	 "eval-pure-explicit-unit.rkt"
	 "sto-monad-unit.rkt"
	 "env-sto-unit.rkt"
         "delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-pure-explicit@ ev@ sto-monad@ δ@ env-sto@))
