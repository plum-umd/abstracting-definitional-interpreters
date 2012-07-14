#lang racket
(provide eval@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"
         "syntax.rkt")

(define-unit eval$@
  (import ev^)
  (export eval^ ev-monad^)
  
  (define (symbolic? x) (or (symbol? x) (pair? x)))
  
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
    (cons (lookup s r x) s))
  
  (define ((alloc f v) s) 
    (match f
      [(cons (lam x e) r)
       (define a (gensym))
       (cons a (update-sto s a v))]))
  
  (define ((new v) s)  
    (define a (gensym))
    (cons a (update-sto s a v)))
  
  (define ((sbox a v) s)
    (cons a (update-sto s a v)))
  
  (define ((ubox a) s)
    (cons (lookup-sto s a) s))
  
  (define ((δ o . vs) s)
    (match* (o vs)
      [('add1 (list (? symbolic? n))) (cons `(add1 ,n) s)]
      [('add1 (list n))  (cons (add1 n) s)]
      [('+ (list (? symbolic? n) m)) (cons `(+ ,n ,m) s)]
      [('+ (list n (? symbolic? m))) (cons `(+ ,n ,m) s)]
      [('+ (list n1 n2)) (cons (+ n1 n2) s)])))

(define-compound-unit/infer eval@
  (import)
  (export EVAL)
  (link [((EVAL : eval^) (EVM : ev-monad^)) eval$@ EV]
        [((EV : ev^)) ev@ EVM]))
