#lang racket
(provide eval aval-set@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"
         "delta-unit.rkt"
         "store.rkt"
         "syntax.rkt")

;; Bounded store abstract set interpreter 

;; eval : E -> [Setof Ans]

(define-unit aval-set@
  (import ev^ δ^)
  (export eval^ ev-monad^ return^)
  
  (define (eval e)
    ((ev e (hash)) (hash)))
  
  (define (rec e r) (ev e r))
 
  (define ((return v) s)
    (return-ans v s))
  
  (define ((bind a f) s)
    (for*/fold ([rs (set)])
      ([ret (a s)])
      (set-union rs
                 (match ret
                   [(cons 'fail s) (set (cons 'fail s))]
                   [(cons v s)
                    ((f v) s)]))))
    
  (define ((return-vals vs) s)
    (return-anss (for/set ([v vs])
                          (cons v s))))
             
  (define (return-ans v s)
    (return-anss (set (cons v s))))
  
  (define (return-anss anss) anss)         
  
  (define ((fail) s)
    (return-ans 'fail s))    
  
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
    ((return-vals (lookup-sto s a)) s)))


(define-values/invoke-unit/infer  
  (link aval-set@ ev@ delta@))
