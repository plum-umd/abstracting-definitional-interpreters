#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../fixed/eval-bang.rkt"
           "../syntax.rkt"
           "../parser.rkt"
           "tests.rkt")
  
  (check-match (eval (dd 0))
               (cons 2 (↦ (i 0) (x 2) (y 11))))
  
  (check-match (eval (dd 1))
               (cons 13
                     (↦ (i 1) (x 7) (y 13))))
  
  (check-match (eval (dd* 0))
               (cons 22
                     (↦ (i 0) (x 2) (y 11))))
  
  (check-match (eval (dd* 1))
               (cons 91
                     (↦ (i 1) (x 7) (y 13))))
  
  (check-match (eval (fact 5))
               (cons 120
                     (↦ (f _) (x0 5) (x1 4) (x2 3) (x4 2) (x5 1) (x6 0))))
  
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push))

  (check-match (eval ref-sref)
        (cons 42
              (↦ (_ 0)))))
