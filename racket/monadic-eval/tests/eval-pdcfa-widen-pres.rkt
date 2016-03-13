#lang racket

(module+ test
  (require rackunit
           racket/set
           "../map.rkt"
           "../fixed/eval-pdcfa-widen-pres.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> (℘(value ∪ (failure)) × σ) × Σ
  (define (get-as-σs out)
    (let ([σ (cdar out)])
      (for/list ([ans (in-set (caar out))])
        (cons ans σ))))

  (test eval (dd '(add1 0)) get-as-σs
        #:answer   91
        #:bindings `("N" ,{set 1})
                   `("x" ,{set 7})
                   `("y" ,{set 13}))
  
  (test eval (fact 5) get-as-σs
        #:answer   'N
        #:bindings `("x" ,{set 'N})
                   `("f" _)
        #:answer   5
        #:bindings `("x" ,{set 'N})
                   `("f" _))
  
  (test eval (fact -1) get-as-σs
        #:answer   'N
        #:bindings `("x" ,{set 'N})
                   `("f" _)
        #:answer   -1
        #:bindings `("x" ,{set 'N})
                   `("f" _))

  (define U (lam (app (vbl 'f) (vbl 'f))))
  (test eval omega get-as-σs)

  (define Uₚ (lam (app (vbl 'f) (app (vbl 'f) (vbl 'f)))))
  (test eval omega-push get-as-σs)

  (test eval ref-sref get-as-σs
        #:answer 42
        #:bindings `(_ ,{set 'N})
        #:answer (failure)
        #:bindings `(_ ,{set 'N})))
