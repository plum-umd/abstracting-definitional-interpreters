#lang racket

(module+ test
  (require rackunit
           racket/set
           "../map.rkt"
           "../fixed/eval-grabbag.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> (℘((value ∪ (failure)) × θ) × σ) × Σ
  (define (get-as-σs out)
    (set (cons (for/set ([v×θ (caar out)]) (car v×θ)) (cdar out))))

  (define ((∈-θ . es) result)
    ;; Checks that each exp in es is dead code.
    (let ([θ (for/fold ([θ {set}]) ([v×θ (caar result)]) (set-union θ (cdr v×θ)))])
      (if (empty? es)
          (set-empty? θ)
          (andmap (λ (e) (set-member? θ e)) es))))

  (test eval (dd '(add1 0)) get-as-σs
        #:timeout 60
        #:answer   {set 13}
        #:bindings
        #:preds
        (∈-θ (ifz (vbl 'input) (num 2) (num 3))
             (num 5)
             (num 11)))

  (test eval (dd 0) get-as-σs
        #:answer   {set 2}
        #:bindings
        #:preds
        (∈-θ (num 3)
             (ifz (vbl 'input) (num 5) (num 7))
             (num 13)))

  (test eval (dd 1) get-as-σs
        #:answer   {set 13}
        #:bindings
        #:preds
        (∈-θ (ifz (vbl 'input) (num 2) (num 3))
             (num 5)
             (num 11)))

  (test eval (dd* '(add1 0)) get-as-σs
        #:answer   {set 91}
        #:bindings
        #:preds
        (∈-θ (ifz (vbl 'input) (num 2) (num 3))
             (num 5)
             (num 11)))

  (test eval (dd* 0) get-as-σs
        #:answer   {set 22}
        #:bindings
        #:preds
        (∈-θ (num 3)
             (ifz (vbl 'input) (num 5) (num 7))
             (num 13)))

  (test eval (dd* 1) get-as-σs
        #:answer   {set 91}
        #:bindings
        #:preds
        (∈-θ (ifz (vbl 'input) (num 2) (num 3))
             (num 5)
             (num 11)))
  
  (test eval (fact 5) get-as-σs
        #:timeout 60
        #:answer   {set 5 'N}
        #:bindings
        #:preds (∈-θ))
  
  (test eval (fact -1) get-as-σs
        #:answer   {set 'N -1}
        #:bindings
        #:preds (∈-θ))

  (define U (lam (app (vbl 'f) (vbl 'f))))
  (test eval omega get-as-σs
        #:answer {set}
        #:bindings
        #:preds (∈-θ))

  (define Uₚ (lam (app (vbl 'f) (app (vbl 'f) (vbl 'f)))))
  (test eval omega-push get-as-σs
        #:answer {set}
        #:bindings
        #:preds (∈-θ)))
