#lang racket/unit
(require racket/match
         racket/set
	 "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt")

(import monad^ return-vals^ return-ans^)
(export env^ sto^ ref^)

(define ext hash-set)

(define (update-sto s a v)
  (hash-set s a (set v)))

(define ((get r x) σ)
  ((return-vals (lookup σ r x)) σ))

(define ((alloc f v) s)
  (match f
    [(cons (lam x e) r)
     (define a (next s))
     (return-ans a (join-sto s a v))]))

(define ((ralloc x e ρ) s)
  (define a (next s))
  (return-ans a (join-sto s a (cons e (hash-set ρ x a)))))

(define ((new v) s)
  (define a (next s))
  (return-ans a (join-sto s a v)))

(define ((sbox a v) s)
  (return-ans a (update-sto s a v)))

(define ((ubox a) s)
  ((return-vals (lookup-sto s a)) s))
