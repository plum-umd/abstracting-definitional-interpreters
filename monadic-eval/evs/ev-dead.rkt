#lang racket/unit
(require racket/set
         "../transformers.rkt"
         "../signatures.rkt")

(import mdead^ monad^)
(export ev-dead^)
(init-depend monad^)

(define-monad M)

(define (((ev-dead ev0) ev) e)
  (do θ  ← get-dead
      θ* ≔ (set-remove θ e)
      (put-dead θ*)
      ((ev0 ev) e)))
