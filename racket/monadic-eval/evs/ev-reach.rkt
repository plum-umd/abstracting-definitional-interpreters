#lang racket/unit
(require racket/match
         racket/set
         "../transformers.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^ mreach^)
(export ev-reach^)

(define (((ev-reach ev0) ev) e)
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
        (tell-reach {set `(,e ,ρ ,σ)})
        ((ev0 ev) e))))
