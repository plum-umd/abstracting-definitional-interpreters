#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-pdcfa-gc-roots.rkt"
           "tests.rkt")

  (check-match (eval (dd* '(add1 0)))
               (cons (set (cons 'N (↦))) cache))
 
  (check-match (eval (dd* 0))
               (cons (set (cons 'N (↦))) cache))

  (check-match (eval (dd* 1))
               (cons (set (cons 'N (↦))) cache))

  (check-match (eval (dd '(add1 0)))
               (cons (set (cons  5 (↦))
                          (cons  7 (↦))
                          (cons  3 (↦))
                          (cons 11 (↦))
                          (cons 13 (↦))
                          (cons  2 (↦)))
                     cache))
  
  (check-match (eval (dd 0))
               (cons (set (cons 2 (↦))) cache))

  (check-match (eval (dd 1))
               (cons (set (cons 13 (↦))) cache))
  
  (check-match (eval (fact 5))
               (cons (set (cons 'N (↦))) cache))
  
  (check-match (eval (fact -1))
               (cons (set (cons 'N (↦))) cache))
  
  (check-match (eval omega)
               (cons (set) cache))
  
  (check-match (eval omega-push)
               (cons (set) cache)))
