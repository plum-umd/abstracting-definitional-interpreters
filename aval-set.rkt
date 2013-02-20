#lang racket
(provide eval aval-set@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"
         "delta-unit.rkt"
         "sto-0cfa-unit.rkt")

;; Bounded store abstract set interpreter 

;; eval : E -> [Setof Ans]

(define-unit aval-set@
  (import ev^)
  (export eval^ ev-monad^ return^ return-ans^ return-vals^)
  
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
    (return-ans 'fail s)))  
  
(define-values/invoke-unit/infer  
  (link aval-set@ ev@ delta@ sto-0cfa@))
