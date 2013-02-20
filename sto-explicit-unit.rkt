#lang racket/unit
(require racket/match
	 "bind-sig.rkt"
	 "fail-sig.rkt"
	 "unit-sig.rkt"
	 "env-sig.rkt"
	 "sto-sig.rkt"
         "store.rkt"
         "syntax.rkt")

(import unit^)
(export env^ sto^)

(define (lookup s r x)
  (hash-ref s (hash-ref r x)))
  
(define ((get r x) s)
  ((unit (lookup s r x)) s))

(define ((alloc f v) s) 
  (match f
    [(cons (lam x e) r)
     (define a (gensym))
     ((unit a) (update-sto s a v))]))

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