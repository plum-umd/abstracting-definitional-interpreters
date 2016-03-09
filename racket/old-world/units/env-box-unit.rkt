#lang racket/unit
(require racket/match
	 "../signatures.rkt")

(import)
(export env^ sto^)

(define env₀ hash)
(define sto₀ void)
(define ext hash-set)

(define (get r x)
  (unbox (hash-ref r x)))

(define (alloc f v) (box v))
(define (ralloc x e ρ)
  (define b (box #f))
  (define f (cons e (hash-set ρ x b)))
  (set-box! b f)
  b)
