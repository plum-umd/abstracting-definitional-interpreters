#lang racket
(provide eval eval@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "ev-unit.rkt"         
         "sto-explicit-unit.rkt"
         "delta-unit.rkt")

(define-unit eval@
  (import ev^ sto-monad^)
  (export eval^ return^ ev-monad^)  
  (define (eval e) ((ev e (hash)) (hash)))  
  (define (rec e r) (ev e r))
  (define ((return v) s) (cons v s))
  (define ((fail) s) (cons 'fail s))
  (define ((bind a f) s)
    (match (a s)
      [(cons 'fail s) (cons 'fail s)]
      [(cons v s)     ((f v) s)])))

(define-values/invoke-unit/infer
  (link eval@ ev@ delta@ sto-explicit@))
