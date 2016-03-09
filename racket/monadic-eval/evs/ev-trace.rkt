#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^ mtrace^)
(export ev-trace^)

(define (((ev-trace ev0) ev) e)
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
        (tell-trace `((,e ,ρ ,σ)))
        ((ev0 ev) e))))
