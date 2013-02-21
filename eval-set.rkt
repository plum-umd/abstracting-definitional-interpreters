#lang racket
(provide eval)
(require "eval-set-unit.rkt"
	 "ev-bang-unit.rkt"
         "store.rkt"
         "delta-unit.rkt"
         "sto-set-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-set@ ev!@ δ@ sto-set@))
