#lang racket/unit
(require racket/match
	 "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt")

(import unit^)
(export sto^)

(define ((ralloc x v) s)
  (match v
    [(cons e r)
     (define a (gensym))
     ((unit a) (update-sto s a (cons e (hash-set r x a))))]))

(define ((new v) s)
  (define a (gensym))
  ((unit a) (update-sto s a v)))

(define ((sbox a v) s)
  ((unit a) (update-sto s a v)))

(define ((ubox a) s)
  ((unit (lookup-sto s a)) s))
