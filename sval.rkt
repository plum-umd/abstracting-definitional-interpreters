#lang racket
(provide eval eval@ (struct-out both-ans))
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "symbolic-monad-sig.rkt"
         "ev-symbolic-unit.rkt"
         "delta-unit.rkt"
         "syntax.rkt")

(struct both-ans (left right) #:transparent)

(define-unit eval@
  (import ev^ δ^)
  (export eval^ return^ ev-monad^ symbolic-monad^)
  
  (define (symbolic? x) (or (symbol? x) (pair? x)))
  
  (define (lookup s r x)
    (hash-ref s (hash-ref r x)))
  
  (define (update-sto s a v)
    (hash-set s a v))
  
  (define (lookup-sto s a)
    (hash-ref s a))
    
  (define ((both v1 v2) s)
    (both-ans (cons v1 s) (cons v2 s)))
  
  (define (symbolic-apply f v)
    (return `(,f ,v)))
  
  (define (eval e)
    ((ev e (hash)) (hash))) 
  
  (define (rec e r) (ev e r))
  (define ((return v) s) (cons v s))
  (define ((fail) s) (cons 'fail s))
  (define ((bind a f) s)
    (define (rec res)
      (match res
        [(both-ans a1 a2)
         (both-ans (rec a1) (rec a2))]
        [(cons 'fail s) (cons 'fail s)]     
        [(cons v s)
         ((f v) s)]))
    (rec (a s)))
      
  
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
  (link eval@ ev-symbolic@ symbolic-delta@))
