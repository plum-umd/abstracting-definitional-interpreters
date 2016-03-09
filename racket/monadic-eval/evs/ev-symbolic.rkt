#lang racket/unit

(require racket/match
         "../signatures.rkt" "../syntax.rkt" "../transformers.rkt")

(import monad^ δ^)
(export ev-symbolic^)

(define (((ev-symbolic ev₀) ev) e)
  (with-monad M
    (match e
      [(sym x)        (return x)]
      [_              ((ev₀ ev) e)])))
