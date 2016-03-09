#lang racket/unit
(require "../transformers.rkt"
         "../signatures.rkt"
         "../subexp.rkt")

(import mdead^ monad^)
(export eval-dead^)

;; eval-dead : (e → M v) → e → M v
(define ((eval-dead eval) e₀)
  (with-monad M
    (do (put-dead (subexps e₀))
        (eval e₀))))
