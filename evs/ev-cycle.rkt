#lang racket/unit
(require "../map.rkt"
         "../transformers.rkt"
         "../signatures.rkt"
         racket/set)

(import monad^ menv^ mstore^ mcycle^)
(export ev-cycle^)
(init-depend monad^)

(define-monad M)

(define (((ev-cycle ev0) ev) e)
  (do ρ ← ask-env
      σ ← get-store
      Σ ← ask-cycle
      (let ([ς (list e ρ σ)])
        (if (set-member? Σ ς)
            mzero
            (local-cycle (set-add Σ ς) ((ev0 ev) e))))))
