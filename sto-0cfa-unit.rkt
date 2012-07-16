#lang racket/unit
(require racket/match
         "ev-monad-sig.rkt"
         "store.rkt"
         "syntax.rkt")

(import return^ return-vals^ return-ans^)
(export sto-monad^)

(define ((lookup-env r x) s)
  ((return-vals (lookup s r x)) s))

(define ((alloc f v) s)
  (match f
    [(cons (lam x e) r)
     (define a x) ; 0CFA-like abstraction
     (return-ans a (join-sto s a v))]))

(define ((new v) s)   
  (define a 'box) ; One box per program abstraction
  (return-ans a (join-sto s a v)))

(define ((sbox a v) s)
  (return-ans a (join-sto s a v)))

(define ((ubox a) s)
  ((return-vals (lookup-sto s a)) s))
