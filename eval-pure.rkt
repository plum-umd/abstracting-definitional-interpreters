#lang racket
(provide eval)
(require "ev-unit.rkt"
	 "eval-pure-unit.rkt"
	 "env-unit.rkt"
         "delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-pure@ ev@ δ@ env@))
