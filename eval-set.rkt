#lang racket
(provide eval eval-set@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"
         "store.rkt"
         "syntax.rkt")

;; Singleton set interpreter 

(define-unit eval-set@
  (import ev^)
  (export eval^ ev-monad^)
  
  (define (eval e)
    ((ev e (hash)) (hash)))
  
  (define (rec e r) (ev e r))
  (define ((return v) s) (set (cons v s)))
  (define ((fail) s) (set (cons 'fail s)))
  (define ((bind a f) s)
    (for*/fold ([rs (set)])
      ([ret (a s)])
      (set-union rs
                 (match ret
                   [(cons 'fail s) (set (cons 'fail s))]
                   [(cons v s)
                    ((f v) s)]))))
  
  (define ((lookup-env r x) s)
    (for/set [(v (lookup s r x))]
             (cons v s)))
  
  (define ((alloc f v) s) 
    (match f
      [(cons (lam x e) r)
       (define a (gensym))
       (set (cons a (join-sto s a v)))]))
  
  (define ((new v) s)  
    (define a (gensym))
    (set (cons a (update-sto s a (set v)))))
  
  (define ((sbox a v) s)
    (set (cons a (update-sto s a (set v)))))
  
  (define ((ubox a) s)
    (for/set ((v (lookup-sto s a)))
             (cons v s)))
  
  (define ((δ o . vs) s)
    (set 
     (match* (o vs)
       [('add1 (list n))  (cons (add1 n) s)]
       [('+ (list n1 n2)) (cons (+ n1 n2) s)]))))

(define-values/invoke-unit/infer  
  (link eval-set@ ev@))
