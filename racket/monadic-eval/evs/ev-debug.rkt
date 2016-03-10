#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt"
         "../syntax.rkt")

(import monad^ mstore^ menv^)
(export ev-debug^)

(define-monad M)

(define (((ev-debug ev0) ev) e)
  (do ρ ← ask-env
    σ ← get-store
    (begin (printf "ev ~a  ~a  ~a\n" e ρ σ)
           ((ev0 ev) e))))
