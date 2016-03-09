#lang racket/unit
(require "../../monad-transformers.rkt"
         "../signatures.rkt"
         "../util/subexp.rkt")

(import mdead^ monad^)
(export eval-dead^)

;; eval-dead : (e → M v) → e → M v
(define ((eval-dead eval) e₀)
  (with-monad M
    (do (put-dead (subexps e₀))
        (eval e₀))))
