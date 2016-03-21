#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../fixed/eval-trace.rkt"
           "../syntax.rkt"
           "tests.rkt")
  
  (check-match (eval (dd 0))
               (cons (cons 2 (↦ (i 0) (x 2) (y 11)))
                     trace))
  
  (check-match (eval (dd 1))
               (cons (cons 13 (↦ (i 1) (x 7) (y 13)))
                     trace))
  
  (check-match (eval (dd* 0))
               (cons (cons 22 (↦ (i 0) (x 2) (y 11)))
                     trace))
  
  (check-match (eval (dd* 1))
               (cons (cons 91 (↦ (i 1) (x 7) (y 13)))
                     trace))
  
  (check-match (eval (fact 5))
               (cons (cons 120 (↦ (f _) (x0 5) (x1 4) (x2 3) (x4 2) (x5 1) (x6 0)))
                     trace))
  
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push)))
