#lang racket

(module+ test
  (require rackunit
           racket/set
           "../map.rkt"
           "../fixed/eval-pdcfa-widen.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> (℘(value ∪ (failure)) × σ) × Σ
  (define (get-as-σs out) (list (cons (caar out) (cdar out))))

  (test eval (dd '(add1 0)) get-as-σs
        #:answer   {set 2 3 5 7 11 13}
        #:bindings `("input" ,{set 'N})
                   `("x" ,{set 3 5 7 2})
                   `("y" ,{set 11 13}))

  (test eval (dd 0) get-as-σs
        #:answer   {set 2}
        #:bindings `("input" ,{set 0})
                   `("x" ,{set 2})
                   `("y" ,{set 11}))

  (test eval (dd 1) get-as-σs
        #:answer   {set 13}
        #:bindings `("input" ,{set 1})
                   `("x" ,{set 7})
                   `("y" ,{set 13}))

  (test eval (dd* '(add1 0)) get-as-σs
        #:answer   {set 'N}
        #:bindings `("input" ,{set 'N})
                   `("x" ,{set 3 5 7 2})
                   `("y" ,{set 11 13}))

  (test eval (dd* 0) get-as-σs
        #:answer   {set 'N}
        #:bindings `("input" ,{set 0})
                   `("x" ,{set 2})
                   `("y" ,{set 11}))

  (test eval (dd* 1) get-as-σs
        #:answer   {set 'N}
        #:bindings `("input" ,{set 1})
                   `("x" ,{set 7})
                   `("y" ,{set 13}))
  
  (test eval (fact 5) get-as-σs
        #:answer   {set 'N}
        #:bindings `("x" ,{set 'N 5})
                   `("f" _))
  
  (test eval (fact -1) get-as-σs
        #:answer   {set 'N}
        #:bindings `("x" ,{set 'N -1})
                   `("f" _))

  (define U (lam (app (vbl 'f) (vbl 'f))))
  (test eval omega get-as-σs
        #:answer {set}
        #:bindings)

  (define Uₚ (lam (app (vbl 'f) (app (vbl 'f) (vbl 'f)))))
  (test eval omega-push get-as-σs
        #:answer {set}
        #:bindings)

  (test eval ref-sref get-as-σs
        #:answer {set 42 (failure)}
        #:bindings `(_ ,{set 0 1})))
