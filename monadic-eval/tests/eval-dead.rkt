#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../fixed/eval-dead.rkt"
           "../syntax.rkt"
           "tests.rkt")

  ;; eval : e → (v × σ) × θ
  (define (get-as-σs x) (list (car x)))

  (define ((∈-θ . es) result)
    ;; Checks that each exp in es is dead code.
    (let ([θ (cdr result)])
      (if (empty? es)
          (set-empty? θ)
          (andmap (λ (e) (set-member? θ e)) es))))

  (test eval (dd* 0) get-as-σs
        #:answer   22
        #:bindings '("input" 0) '("x" 2) '("y" 11)
        #:preds
        (∈-θ (num 3)
             (ifz (vbl 'input) (num 5) (num 7))
             (num 13)))
  
  (test eval (dd* 1) get-as-σs
        #:answer   91
        #:bindings '("input" 1) '("x" 7) '("y" 13)
        #:preds
        (∈-θ (ifz (vbl 'input) (num 2) (num 3))
             (num 5)
             (num 11)))
  
  (test eval (fact 5) get-as-σs
        #:answer   120
        #:bindings
         '("x" 5) '("x" 4) '("x" 3) '("x" 2)
         '("x" 1) '("x" 0) '("f" _)
        #:preds
        (∈-θ))
  
  (test eval (fact -1) DIVERGES)

  (test eval omega DIVERGES)

  (test eval omega-push DIVERGES)

  (test eval ref-sref get-as-σs
        #:answer   42
        #:bindings '(_ 0)
        #:preds
        (∈-θ 'err)))
