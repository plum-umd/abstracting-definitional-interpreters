#lang racket
(provide eval)
(require "ev-unit.rkt"
	 "eval-pure-unit.rkt"
	 "id-monad-unit.rkt"
	 "env-box-unit.rkt"
         "delta-unit.rkt")

(define-values/invoke-unit/infer
  (link eval-pure@ ev@ id-monad@ δ@ env-box@))
