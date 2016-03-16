#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-nd-abs.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> ℘((value ∪ (failure) × σ))
  (define (get-as-σs x) x)

  (test eval (dd* 0) get-as-σs
        #:answer   'N
        #:bindings `("input" ,{set 0}) `("x" ,{set 2}) `("y" ,{set 11}))
  
  
  (test eval (dd* 1) get-as-σs
        #:answer   'N
        #:bindings `("input" ,{set 1}) `("x" ,{set 7}) `("y" ,{set 13}))
  
  (test eval (dd* '(add1 0)) get-as-σs
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 7})
        `("y" ,{set 11})
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 5})
        `("y" ,{set 13})
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 2})
        `("y" ,{set 11})
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 3})
        `("y" ,{set 11})
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 5})
        `("y" ,{set 11})
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 2})
        `("y" ,{set 13})
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 3})
        `("y" ,{set 13})
        #:answer   'N
        #:bindings `("input" ,{set 'N})
        `("x" ,{set 7})
        `("y" ,{set 13}))
  
  (test eval (fact 5) DIVERGES)

  (test eval (fact -1) DIVERGES)

  (test eval omega DIVERGES)

  (test eval omega-push DIVERGES)

  (test eval ref-sref get-as-σs
        #:answer  (failure)
        #:bindings `(_ ,{set 'N})
        #:answer  42
        #:bindings `(_ ,{set 'N})))
