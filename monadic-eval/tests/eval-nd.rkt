#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-nd.rkt"
           "tests.rkt")

  (check-match (eval (dd* 0))
               (set (cons 22
                          (↦ (i (set 0)) (x (set 2)) (y (set 11))))))
 
  (check-match (eval (dd* 1))
               (set (cons 91
                          (↦ (i (set 1)) (x (set 7)) (y (set 13))))))

  (check-match (eval (fact 5))
               (set (cons 120
                          (↦ (x0 (set 5))
                             (x1 (set 4))
                             (x2 (set 3))
                             (x3 (set 2))
                             (x4 (set 1))
                             (x5 (set 0))
                             (f  (set _))))))
  
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push))
  
  (check-match (eval ref-sref)
               (set (cons 'failure (↦ (x (set 0 1))))
                    (cons 42       (↦ (y (set 0 1)))))))
