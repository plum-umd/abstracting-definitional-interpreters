#lang racket/unit
(require "../transformers.rkt"
         "../signatures.rkt"
         "../syntax.rkt")

(import monad^ menv^ mstore^)
(export ev-echo^)
(init-depend monad^)

(define-monad M)

(define (((ev-echo ev0) ev) e)
  (do ρ ← ask-env
      σ ← get-store
      (begin (printf "eval ~a ~a\n" (pp e) σ)
             ((ev0 ev) e))))
