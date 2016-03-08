#lang racket/unit
(require racket/match
         "../../monad-transformers.rkt"
         "../signatures.rkt"
         "../units/ev-base.rkt"
         "../units/delta-con.rkt"
         "../units/st-explicit.rkt"
         "../monad/trace.rkt")

(import (rename ev^ [ev0 ev]) monad^ env^ store^ trace^)
(export ev^)

(define ((ev ev) e)
  (do ρ ← ask-env
      σ ← get-store
      τ ← get-trace
      (put-trace (list e ρ σ))
      (ev0 e)))
