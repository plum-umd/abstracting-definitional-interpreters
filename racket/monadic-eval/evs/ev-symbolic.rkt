#lang racket/unit

(require racket/match
         "../signatures.rkt" "../syntax.rkt" "../transformers.rkt")

(import monad^ δ^)
(export ev-symbolic^)

(define (((ev-symbolic ev₀) ev) e)
  (with-monad M
    (match e
      [(sym x) (case x ; reserve `N` for (unrefinable) abstract integer
                 [(N) (error 'ev-symbolic "Syntax error: symbolic name must not be `N`")]
                 [else (return x)])]
      [_       ((ev₀ ev) e)])))
