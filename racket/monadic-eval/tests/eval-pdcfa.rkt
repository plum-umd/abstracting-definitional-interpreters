#lang racket

(module+ test
  (require rackunit
           racket/set
           "../map.rkt"
           "../fixed/eval-pdcfa.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> ℘((value ∪ (failure)) × σ) × Σ
  (define get-as-σs car)

  (test eval (dd '(add1 0)) get-as-σs
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 7})
                   `("y" ,{set 11})
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 5})
                   `("y" ,{set 13})
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 2})
                   `("y" ,{set 11})
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 3})
                   `("y" ,{set 11})
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 5})
                   `("y" ,{set 11})
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 2})
                   `("y" ,{set 13})
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 3})
                   `("y" ,{set 13})
        #:answer   'N
        #:bindings `("N" ,{set 'N})
                   `("x" ,{set 7})
                   `("y" ,{set 13}))
  
  (test eval (fact 5) get-as-σs
        #:answer   'N
        #:bindings `("x" ,{set 'N 5})
                   `("f" _))
  
  (test eval (fact -1) get-as-σs
        #:answer   'N
        #:bindings `("x" ,{set 'N -1})
                   `("f" _) 1)

  (define U (lam (app (vbl 'f) (vbl 'f))))
  (test eval omega get-as-σs)

  (define Uₚ (lam (app (vbl 'f) (app (vbl 'f) (vbl 'f)))))
  (test eval omega-push get-as-σs)

  (test eval ref-sref get-as-σs
        #:answer 42
        #:bindings `(_ ,{set 0 1})
        #:answer (failure)
        #:bindings `(_ ,{set 0 1}))

)
