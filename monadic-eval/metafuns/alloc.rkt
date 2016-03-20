#lang racket/unit

(require "../signatures.rkt"
         "../transformers.rkt"
	 "../map.rkt")

(import monad^ mstore^)
(export alloc^)

(define-monad M)

(define (next σ)
  (add1 (apply max -1 (keys σ))))

(define (alloc _)
  (do σ ← get-store
    (return (next σ))))

