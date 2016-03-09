#lang racket/unit

(require racket/match
         "../signatures.rkt" "../syntax.rkt" "../transformers.rkt")

(import monad^ δ^)
(export ev-symbolic^)

(define (((ev-symbolic ev₀) ev) e)
  (with-monad M
    (match e
      [(sym x)        (return x)]
      [(ifz e0 e1 e2) (do v ← (ev e0)
                          n ← (δ 'flip v)
                          (case n
                            [(0) (ev e2)]
                            [(1) (ev e1)]))]
      [_              ((ev₀ ev) e)])))
