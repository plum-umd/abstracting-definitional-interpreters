#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-dead.rkt"
           "../syntax.rkt"
           "tests.rkt")

  (check-match (eval (dd* 0))
               (cons (cons 22 (↦ (i 0) (x 2) (y 11)))
                     (set (num 3)
                          (num 13)
                          (ifz (vbl 'input) (num 5) (num 7))
                          ;(vbl 'input) ; b/c identical expr is live
                          (num 5)
                          (num 7))))
  
  (check-match (eval (dd* 1))
               (cons (cons 91 (↦ (i 1) (x 7) (y 13)))
                     (set (ifz (vbl 'input) (num 2) (num 3))
                          (num 5)
                          (num 11)
                          (num 2)
                          (num 3))))
  
  (check-match (eval (fact 5))
               (cons (cons 120 (↦ (f _) (x0 5) (x1 4) (x2 3) (x4 2) (x5 1) (x6 0)))
                     (set)))
  
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push))
  
  (check-match (eval ref-sref)
               (cons (cons 42 (↦ (_ 0)))
                     (set 'err))))
