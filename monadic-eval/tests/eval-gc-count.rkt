#lang racket

(module+ test
  (require rackunit
           racket/set
           racket/syntax
           "../map.rkt"
           "../fixed/eval-gc-count.rkt"
           "../syntax.rkt"
           "../parser.rkt"
           "tests.rkt")
  
  (check-match (eval (dd 0))
               (cons (cons 2 (↦)) (↦)))

  (check-match (eval (dd 1))
               (cons (cons 13 (↦)) (↦)))
  
  (check-match (eval (dd* 0))
               (cons (cons 22 (↦)) (↦)))
  
  (check-match (eval (dd* 1))
               (cons (cons 91 (↦)) (↦)))
  
  (check-match (eval (fact 5))
               (cons (cons 120 (↦)) (↦)))
  
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push))
  
  (check-match (eval ref-sref)
               (cons (cons 42 (↦)) (↦))))
