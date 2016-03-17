#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^)
(export ev-trace^)

(define-monad M)

(define (((ev-trace ev0) ev) e)
  (do ρ ← ask-env
      σ ← get-store
      (tell `((,e ,ρ ,σ)))
      ((ev0 ev) e)))
