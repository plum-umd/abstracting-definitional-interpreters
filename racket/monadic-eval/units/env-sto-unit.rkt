#lang racket/unit
(require racket/match
	 "../syntax.rkt"
	 "../store.rkt"
	 "../signatures.rkt")

(import unit^)
(export env^)

(define ((get ρ x) σ)
  ((unit (hash-ref σ (hash-ref ρ x))) σ))

(define ((alloc f v) σ)
  (match f
    [(cons (lam x e) ρ)
     (define a (next σ))
     ((unit a) (update-sto σ a v))]))

(define ((ralloc x v) σ)
  (match v
    [(cons e ρ)
     (define a (next σ))
     ((unit a)
      (update-sto σ a
        (cons e (hash-set ρ x a))))]))

