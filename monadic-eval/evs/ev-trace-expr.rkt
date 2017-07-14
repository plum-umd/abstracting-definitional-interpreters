#lang racket/unit
(require racket/match
         "../transformers.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^)
(export ev-trace-expr^)

(define-monad M)

(define (((ev-trace-expr ev0) ev) e)
  (do (tell e)
      ((ev0 ev) e)))
