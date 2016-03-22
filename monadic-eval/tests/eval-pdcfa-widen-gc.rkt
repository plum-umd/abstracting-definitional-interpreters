#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-pdcfa-widen-gc.rkt"
           "tests.rkt")

  (check-match (eval (dd* '(add1 0)))
               (cons (cons (set 'N) (↦)) cache))
 
  (check-match (eval (dd* 0))
               (cons (cons (set 'N) (↦)) cache))

  (check-match (eval (dd* 1))
               (cons (cons (set 'N) (↦)) cache))

  (check-match (eval (dd '(add1 0)))
               (cons (cons (set  5 7 3 11 13 2) (↦))
                     cache))

  (check-match (eval (dd 0))
               (cons (cons (set 2) (↦)) cache))

  (check-match (eval (dd 1))
               (cons (cons (set 13) (↦)) cache))
  
  (check-match (eval (fact 5))
               (cons (cons (set 'N) (↦)) cache))
  
  (check-match (eval (fact -1))
               (cons (cons (set 'N) (↦)) cache))
  
  (check-match (eval omega)
               (cons (cons (set) (↦)) cache))
  
  (check-match (eval omega-push)
               (cons (cons (set) (↦)) cache)))
