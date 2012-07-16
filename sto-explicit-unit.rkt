#lang racket/unit
(require racket/match
         "ev-monad-sig.rkt"
         "store.rkt"
         "syntax.rkt")

(import return^)
(export sto-monad^)

(define (lookup s r x)
  (hash-ref s (hash-ref r x)))
  
(define ((lookup-env r x) s)
  ((return (lookup s r x)) s))

(define ((alloc f v) s) 
  (match f
    [(cons (lam x e) r)
     (define a (gensym))
     ((return a) (update-sto s a v))]))

(define ((new v) s)  
  (define a (gensym))
  ((return a) (update-sto s a v)))

(define ((sbox a v) s)
  ((return a) (update-sto s a v)))

(define ((ubox a) s)
  ((return (lookup-sto s a)) s))