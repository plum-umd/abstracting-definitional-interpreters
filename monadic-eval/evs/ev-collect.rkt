#lang racket/unit
(require racket/set
         "../signatures.rkt"
         "../transformers.rkt"
         "../gc.rkt")

(import monad^ gc^ mstore^)
(export ev-collect^)
(init-depend monad^)

(define-monad M)

(define (((ev-collect ev0) ev) e)
  (do ψ ← ask-roots
      v ← ((ev0 ev) e)
      (update-store (gc (set-union ψ (roots-v v))))
      (return v)))

