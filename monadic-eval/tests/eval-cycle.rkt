#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-cycle.rkt"
           "tests.rkt")

  (check-match (eval (dd 0))
               (set (cons 2 (↦ (i 0) (x 2) (y 11)))))
  
  (check-match (eval (dd 1))
               (set (cons 13
                          (↦ (i 1) (x 7) (y 13)))))
  
  (check-match (eval (dd* 0))
               (set (cons 22
                          (↦ (i 0) (x 2) (y 11)))))
  
  (check-match (eval (dd* 1))
               (set (cons 91
                          (↦ (i 1) (x 7) (y 13)))))
  
  (check-match (eval (fact 5))
               (set (cons 120
                          (↦ (f _) (x 0)))))
      
  (check-diverge (eval (fact -1)))
  (check-match (eval omega) (set))
  (check-match (eval omega-push) (set))

  (check-match (eval ref-sref)
               (set (cons  42
                           (↦ (_ 0))))))
