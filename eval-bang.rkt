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
  
  (define (lookup-env r x)
    (return (hash-ref r x)))
  
  (define (alloc f v)
    (return v))
  
  (define (new v)
    (return (box v)))
  
  (define (sbox a v)
    (set-box! a v)
    (return a))
  
  (define (ubox a)
    (return (unbox a))))

(define-values/invoke-unit/infer
  (link eval@ ev@ delta@))
