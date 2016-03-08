#lang racket/unit
(require racket/match
         racket/set
         "../../monad-transformers.rkt"
         "../signatures.rkt")

(import monad^ env^ store^ reach^)
(export oev^)

(define (((oev ev0) ev) e)
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
        (tell-reach {set `(,e ,ρ ,σ)})
        ((ev0 ev) e))))
