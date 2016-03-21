#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-pdcfa-widen-pres.rkt"
           "tests.rkt")
 
  (check-match (eval (dd '(add1 0)))
               (cons (cons (set 13)
                           (↦ (i (set 1)) (x (set 7)) (y (set 13))))
                     cache))

  (check-match (eval (dd 0))
               (cons (cons (set 2)
                           (↦ (i (set 0))
                              (x (set 2))
                              (y (set 11))))
                     cache))
  
  (check-match (eval (dd 1))
               (cons (cons (set 13)
                           (↦ (i (set 1))
                              (x (set 7))
                              (y (set 13))))
                     cache))

  (check-match (eval (dd* '(add1 0)))
               (cons (cons (set 91)
                           (↦ (i (set 1)) (x (set 7)) (y (set 13))))
                     cache))
  
  (check-match (eval (dd* 0))
               (cons (cons (set 22)
                           (↦ (i (set 0)) (x (set 2)) (y (set 11))))
                     cache))
  
  (check-match (eval (dd* 1))
               (cons (cons (set 91)
                           (↦ (i (set 1)) (x (set 7)) (y (set 13))))
                     cache))
  
  (check-match (eval (fact 5))
               (cons (cons (set 'N 5)
                           (↦ (x (set 'N)) (f _)))
                     cache))
  
  (check-match (eval (fact -1))
               (cons (cons (set 'N -1)
                           (↦ (x (set 'N)) (f _)))
                     cache))

  (check-match (eval omega) (cons (cons (set) (↦))  cache))
  (check-match (eval omega-push) (cons (cons (set) (↦)) cache))

  (check-match (eval ref-sref)
               (cons (cons (set 42 'failure) (↦ (_ (set 'N))))
                     cache)))
