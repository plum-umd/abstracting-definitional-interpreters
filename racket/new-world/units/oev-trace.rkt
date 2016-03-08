#lang racket/unit
(require racket/match
         "../../monad-transformers.rkt"
         "../signatures.rkt")

(import monad^ env^ store^ trace^)
(export oev^)

(define (((oev ev0) ev) e)
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
        (tell-trace `((,e ,ρ ,σ)))
        ((ev0 ev) e))))
