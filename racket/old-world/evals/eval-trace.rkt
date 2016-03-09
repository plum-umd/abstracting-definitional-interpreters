#lang racket
(provide eval)
(require "../fix.rkt"
         "../units/ev-bang-unit.rkt"
         "../units/trace-monad-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/sto-unit.rkt"
         "../units/err-unit.rkt"
         "../signatures.rkt")

(define-values/invoke-unit/infer
  (link ev!@ δ@ trace-monad@ sto@ err@))

(define ((((ev-traces e ρ) ev-traces) σ) τ)
  ((((ev e ρ) ev-traces) σ)
   (cons (list e ρ σ) τ)))
 
(define (eval e)
  (mrun ((fix ev-traces) e (hash))))
