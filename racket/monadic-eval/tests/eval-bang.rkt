#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../fixed/eval-bang.rkt"
           "../syntax.rkt"
           "../parser.rkt"
           "tests.rkt")

  ;; eval : e → v × σ
  (define get-as-σs list)

  (test eval (dd 0) get-as-σs
        #:answer   22
        #:bindings '("N" 0) '("x" 2) '("y" 11))
  
  (test eval (dd 1) get-as-σs
        #:answer   91
         #:bindings '("N" 1) '("x" 7) '("y" 13))
  
  (test eval (fact 5) get-as-σs
        #:answer   120
         #:bindings '("x" 5) '("x" 4) '("x" 3) '("x" 2) '("x" 1) '("x" 0)
         `("f" ,(cons (parse '(λ (x) (if0 x 1 (* x (f (- x 1))))))
                      (∅ 'f "f_6"))))
  
  (test eval (fact -1) DIVERGES)

  (test eval omega DIVERGES)

  (test eval omega-push DIVERGES)

  (test eval ref-sref get-as-σs
        #:answer   42
         #:bindings '(_ 0)))
