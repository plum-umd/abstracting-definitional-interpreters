#lang racket/unit
(require racket/match
         "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt")

(import unit^ unit-vals^ unit-ans^)
(export env^ sto^ ref^)

(define ext hash-set)

(define ((get r x) s)
  ((unit-vals (lookup s r x)) s))

(define ((alloc f v) s)
  (match f
    [(cons (lam x e) r)
     (define a x) ; 0CFA-like abstraction
     (unit-ans a (join-sto s a v))]))

(define ((ralloc x e ρ) s)
  (define a x)
  ((unit a) 
   (join-sto s a
             (cons e (hash-set ρ x a)))))

(define ((new v) s)
  (define a 'box) ; One box per program abstraction
  (unit-ans a (join-sto s a v)))

(define ((sbox a v) s)
  (unit-ans a (join-sto s a v)))

(define ((ubox a) s)
  ((unit-vals (lookup-sto s a)) s))
