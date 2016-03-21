#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
         "../map.rkt")

(import monad^ mstore^)
(export alloc^)

(define-monad M)

;; Relies on invariant that memory is never freed.
;; Unsafe with GC.
(define (alloc _)
  (do σ ← get-store
    (return (size σ))))

