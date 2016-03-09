#lang racket/unit
(require racket/match
         racket/set
         "../syntax.rkt"
	 "../signatures.rkt")

(import)
(export env^ sto^)

(define ext hash-set)

;; Maybe define in terms of `unit'?

(define ((get ρ x) σ)
  {set (cons (hash-ref σ (hash-ref ρ x)) σ)})

(define ((alloc f v) σ)
  (define a (next σ))
  {set (cons a (hash-set σ a v))})

(define ((ralloc x e ρ) σ)
  (define a (next σ))
  {set (cons a
             (hash-set σ a
                       (cons e (hash-set ρ x a))))})

(define (next s)
  (for/fold ([a 0])
            ([i (in-hash-keys s)])
    (max a (add1 i))))
