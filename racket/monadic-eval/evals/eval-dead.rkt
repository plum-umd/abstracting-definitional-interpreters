#lang racket
(provide eval)
(require "../fix.rkt"
         "../signatures.rkt"
         "../subexp.rkt"
         "../units/ev-unit.rkt"
         "../units/dead-monad-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/sto-unit.rkt"
         "../units/err-unit.rkt")

(define-values/invoke-unit/infer
  (link ev@ δ@ dead-monad@ sto@ err@))

(define ((((ev-dead e ρ) ev-dead) σ) τ)
  ((((ev e ρ) ev-dead) σ)
   (set-remove τ e)))

;; FIXME: rewrite with mrun.  Put e in monad?
(define (eval e)
  ((((fix ev-dead) e (hash)) (hash)) (subexps e)))

