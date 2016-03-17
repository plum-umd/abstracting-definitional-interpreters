#lang racket/unit

(require "../signatures.rkt"
         "../transformers.rkt"
	 "../map.rkt")

(import monad^ mstore^)
(export alloc^)

(define-monad M)

(define (alloc _)
  (do σ ← get-store
    (return (size σ))))

