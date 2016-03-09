#lang racket/unit
(require "../signatures.rkt")

(import)
(export env^ sto^)

(define get hash-ref)
(define ext hash-set)

(define (alloc f v) v)
(define (ralloc x e r)
  (define p (make-placeholder #f))
  (define f (cons e (hash-set r x p)))
  (placeholder-set! p f)
  (make-reader-graph f))
