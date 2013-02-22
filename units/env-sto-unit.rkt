#lang racket/unit
(require racket/match
	 "../syntax.rkt"
	 "../store.rkt"
	 "../signatures.rkt")

(import unit^)
(export env^)

(define ((get r x) s)
  ((unit (hash-ref s (hash-ref r x))) s))

(define ((alloc f v) s)
  (match f
    [(cons (lam x e) r)
     (define a (gensym))
     ((unit a)
      (update-sto s a v))]))

(define ((ralloc x v) s)
  (match v
    [(cons e r)
     (define a (gensym))
     ((unit a)
      (update-sto s a
        (cons e (hash-set r x a))))]))

