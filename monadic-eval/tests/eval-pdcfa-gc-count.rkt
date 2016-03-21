#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "../fixed/eval-pdcfa-gc-count.rkt"
           "tests.rkt")

  (check-match (eval (dd* '(add1 0)))
               (cons (set (cons (cons 'N (↦)) (↦))) cache))
 
  (check-match (eval (dd* 0))
               (cons (set (cons (cons 'N (↦)) (↦))) cache))

  (check-match (eval (dd* 1))
               (cons (set (cons (cons 'N (↦)) (↦))) cache))

  (check-match (eval (dd '(add1 0)))
               (cons (set (cons (cons  5 (↦)) (↦))
                          (cons (cons  7 (↦)) (↦))
                          (cons (cons  3 (↦)) (↦))
                          (cons (cons 11 (↦)) (↦))
                          (cons (cons 13 (↦)) (↦))
                          (cons (cons  2 (↦)) (↦)))
                     cache))
  
  (check-match (eval (dd 0))
               (cons (set (cons (cons 2 (↦)) (↦))) cache))

  (check-match (eval (dd 1))
               (cons (set (cons (cons 13 (↦)) (↦))) cache))
  
  (check-match (eval (fact 5))
               (cons (set (cons (cons 'N (↦)) (↦))) cache))
  
  (check-match (eval (fact -1))
               (cons (set (cons (cons 'N (↦)) (↦))) cache))
  
  (check-match (eval omega)
               (cons (set) cache))
  
  (check-match (eval omega-push)
               (cons (set) cache))
  
  (check-match (eval ref-sref)
               (cons (set (cons (cons 42 (↦)) (↦))
                          (cons (cons (failure) (↦)) (↦)))
                     cache)))
