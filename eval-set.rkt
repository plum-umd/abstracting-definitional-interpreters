#lang racket
(provide eval eval-set@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"         
         "store.rkt"
         "delta-unit.rkt"
         "sto-set-unit.rkt"
         "syntax.rkt")

;; Singleton set interpreter 

(define-unit eval-set@
  (import ev^ sto-monad^)
  (export eval^ ev-monad^ return^ return-ans^ return-vals^)
  
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
  
  (define ((return-vals vs) s)
    (return-anss (for/set ([v vs])
                          (cons v s))))
             
  (define (return-ans v s)
    (return-anss (set (cons v s))))
  
  (define (return-anss anss) anss))
  
(define-values/invoke-unit/infer  
  (link eval-set@ ev@ delta@ sto-set@))
