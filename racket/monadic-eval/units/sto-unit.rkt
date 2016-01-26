#lang racket/unit
(require racket/match
	 "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt")

(import unit^)
(export env^ sto^ ref^)

(define sto₀ hash)
(define env₀ hash)
(define ext hash-set)
(define ((get ρ x) σ)
  ((unit (hash-ref σ (hash-ref ρ x))) σ))

(define ((alloc f v) σ)
  (define a (next σ))
  ((unit a) (hash-set σ a v)))

(define ((ralloc x e ρ) s)
  (define a (next s))
  ((unit a) (update-sto s a (cons e (hash-set ρ x a)))))

(define ((new v) s)
  (define a (next s))
  ((unit a) (update-sto s a v)))

(define ((sbox a v) s)
  ((unit a) (update-sto s a v)))

(define ((ubox a) s)
  ((unit (lookup-sto s a)) s))
