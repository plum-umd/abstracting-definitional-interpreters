#lang racket

(module+ test
  (require rackunit
           racket/set
           "../map.rkt"
           "../fixed/eval-pdcfa-gc.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> ℘((value ∪ (failure)) × σ) × Σ
  (define get-as-σs car)

  (test eval (dd* '(add1 0)) get-as-σs
        #:timeout  60
        #:answer   'N
        #:bindings )

  (test eval (dd* 0) get-as-σs
        #:answer   'N
        #:bindings )

  (test eval (dd* 1) get-as-σs
        #:answer   'N
        #:bindings )

  (test eval (dd '(add1 0)) get-as-σs
        #:timeout  60
        #:answer   5
        #:bindings 
        #:answer   7
        #:bindings 
        #:answer   3
        #:bindings 
        #:answer   11
        #:bindings 
        #:answer   13
        #:bindings 
        #:answer   2
        #:bindings )

  (test eval (dd 0) get-as-σs
        #:answer   2
        #:bindings )

  (test eval (dd 1) get-as-σs
        #:answer   13
        #:bindings )

  (test eval (fact 5) get-as-σs
        #:timeout  60
        #:answer   'N
        #:bindings )

  (test eval (fact -1) get-as-σs
        #:timeout  60
        #:answer   'N
        #:bindings )

  (define U (lam (app (vbl 'f) (vbl 'f))))
  (test eval omega get-as-σs)

  (define Uₚ (lam (app (vbl 'f) (app (vbl 'f) (vbl 'f)))))
  (test eval omega-push get-as-σs)

  )
