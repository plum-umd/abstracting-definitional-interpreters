#lang racket
(provide eval)
(require "aval-set-unit.rkt"
	 "ev-bang-unit.rkt"
         "delta-unit.rkt"
         "sto-0cfa-unit.rkt")

(define-values/invoke-unit/infer
  (link aval-set@ ev!@ δ@ sto-0cfa@))
