#lang racket
(provide eval eval@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"
         "delta-unit.rkt"
         "syntax.rkt")

(define-unit eval@
  (import ev^ δ^)
  (export eval^ return^ ev-monad^)
   
  (define (lookup s r x)
    (hash-ref s (hash-ref r x)))
  
  (define (update-sto s a v)
    (hash-set s a v))
  
  (define (lookup-sto s a)
    (hash-ref s a))
  
  (define (eval e)
    ((ev e (hash)) (hash)))  
  
  (define (rec e r) (ev e r))
  (define ((return v) s) (cons v s))
  (define ((fail) s) (cons 'fail s))
  (define ((bind a f) s)
    (match (a s)
      [(cons 'fail s) (cons 'fail s)]
      [(cons v s)
       ((f v) s)]))   
  
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
    (cons (lookup-sto s a) s)))

(define-values/invoke-unit/infer
  (link eval@ ev@ delta@))
