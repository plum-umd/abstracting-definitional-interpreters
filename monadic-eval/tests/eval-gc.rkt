#lang racket

(module+ test
  (require rackunit
           racket/set
           "../map.rkt"
           "../fixed/eval-gc.rkt"
           "../syntax.rkt"
           "../parser.rkt"
           "tests.rkt")
  
  (check-match (eval (dd 0))
               (cons 2 (↦)))
  
  (check-match (eval (dd 1))
               (cons 13 (↦)))
  
  (check-match (eval (dd* 0))
               (cons 22 (↦)))
  
  (check-match (eval (dd* 1))
               (cons 91 (↦)))
  
  (check-match (eval (fact 5))
               (cons 120 (↦)))
  
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push)))
