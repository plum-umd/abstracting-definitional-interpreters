#lang racket
(provide eval eval-set@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
	 "unit-sig.rkt"
         "ev-unit.rkt"
         "store.rkt"
         "delta-unit.rkt"
         "sto-set-unit.rkt"
         "syntax.rkt")

;; Singleton set interpreter

(define-unit eval-set@
  (import ev^)
  (export eval^ ev-monad^ unit^ unit-ans^ unit-vals^)

  (define (eval e)
    ((ev e (hash)) (hash)))

  (define (rec e r) (ev e r))
  (define ((unit v) s) (set (cons v s)))
  (define ((fail) s) (set (cons 'fail s)))
  (define ((bind a f) s)
    (for*/fold ([rs (set)])
      ([ret (a s)])
      (set-union rs
                 (match ret
                   [(cons 'fail s) (set (cons 'fail s))]
                   [(cons v s)
                    ((f v) s)]))))

  (define ((unit-vals vs) s)
    (unit-anss (for/set ([v vs])
                          (cons v s))))

  (define (unit-ans v s)
    (unit-anss (set (cons v s))))

  (define (unit-anss anss) anss))

(define-values/invoke-unit/infer
  (link eval-set@ ev@ δ@ sto-set@))
