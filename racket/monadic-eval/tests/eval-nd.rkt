#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-nd.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> ℘((value ∪ (failure) × σ))
  (define (get-as-σs x) x)

  (test eval (dd 0) get-as-σs
        #:answer   22
        #:bindings `("N" ,{set 0}) `("x" ,{set 2}) `("y" ,{set 11}))
  
  (test eval (dd 1) get-as-σs
        #:answer   91
        #:bindings `("N" ,{set 1}) `("x" ,{set 7}) `("y" ,{set 13}))
  
  (test eval (fact 5) get-as-σs
        #:answer   120
        #:bindings `("x" ,{set 5}) `("x" ,{set 4}) `("x" ,{set 3})
                    `("x" ,{set 2}) `("x" ,{set 1}) `("x" ,{set 0})
                    `("f" _))
  
  (test eval (fact -1) DIVERGES)

  (test eval omega DIVERGES)

  (test eval omega-push DIVERGES)

  (test eval ref-sref get-as-σs
        #:answer  (failure)
        #:bindings `(_ ,{set 0 1})
        #:answer  42
        #:bindings `(_ ,{set 0 1})))
