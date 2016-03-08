#lang racket/unit
(require racket/match
         racket/set
         "../../monad-transformers.rkt"
         "../fix.rkt"
         "../signatures.rkt"
         "../subexp.rkt")

(import monad^ env^ store^ dead^)
(export oev^)

(define (((without-subexp ev0) ev) e)
  (with-monad M
    (do θ  ← get-dead
        ρ  ← ask-env
        σ  ← get-store
        θ* ≔ (set-remove θ e)
        (put-dead θ*)
      ((ev0 ev) e))))

(define (((oev ev0) _) e0)
  (with-monad M
    (do (put-dead (subexps e0))
        ((fix (without-subexp ev0)) e0))))
