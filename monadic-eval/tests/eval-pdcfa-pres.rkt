#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-pdcfa-pres.rkt"
           "tests.rkt")

  (check-match (eval (dd '(add1 0)))
               (cons (set (cons 13 (↦ (i (set 1)) (x (set 7)) (y (set 13)))))
                     cache))
  
  (check-match (eval (dd 0))
               (cons (set (cons 2 (↦ (i (set 0)) (x (set 2)) (y (set 11)))))
                     cache))
 
  (check-match (eval (dd 1))
               (cons (set (cons 13 (↦ (i (set 1)) (x (set 7)) (y (set 13)))))
                     cache))

  (check-match (eval (dd* '(add1 0)))
               (cons (set (cons 91 (↦ (i (set 1)) (x (set 7)) (y (set 13)))))
                     cache))

  (check-match (eval (dd* 0))
               (cons (set (cons 22 (↦ (i (set 0)) (x (set 2)) (y (set 11)))))
                     cache))

  (check-match (eval (dd* 1))
               (cons (set (cons 91 (↦ (i (set 1)) (x (set 7)) (y (set 13)))))
                     cache))

  (check-match (eval (fact 5))
               (cons (set (cons 5  (↦ (x0 (set 'N)) (f0 _)))
                          (cons 'N (↦ (x1 (set 'N)) (f1 _))))
                     cache))

  (check-match (eval (fact -1))
               (cons (set (cons 'N (↦ (x0 (set 'N)) (f0 _)))
                          (cons -1 (↦ (x1 (set 'N)) (f1 _))))
                     cache))
  
  (check-match (eval omega)
               (cons (set) cache))

  (check-match (eval omega-push)
               (cons (set) cache))

  (check-match (eval ref-sref)
               (cons (set (cons 42       (↦ (_ (set 'N))))
                          (cons 'failure (↦ (_ (set 'N)))))
                     cache)))
