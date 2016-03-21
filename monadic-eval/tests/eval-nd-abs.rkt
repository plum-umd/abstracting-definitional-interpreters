#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-nd-abs.rkt"
           "tests.rkt")

  (check-match (eval (dd* 0))
               (set (cons 'N
                          (↦ (i (set 0)) (x (set 2)) (y (set 11))))))

  (check-match (eval (dd* 1))
               (set (cons 'N
                          (↦ (i (set 1)) (x (set 7)) (y (set 13))))))
  
  (check-match (eval (dd* '(add1 0)))
               (set (cons 'N
                          (↦ (i0 (set 'N)) (x0 (set 7)) (y0 (set 11))))
                    (cons 'N
                          (↦ (i1 (set 'N)) (x1 (set 5)) (y1 (set 13))))
                    (cons 'N
                          (↦ (i2 (set 'N)) (x2 (set 2)) (y2 (set 11))))
                    (cons 'N
                          (↦ (i3 (set 'N)) (x3 (set 3)) (y3 (set 11))))
                    (cons 'N
                          (↦ (i4 (set 'N)) (x4 (set 5)) (y4 (set 11))))
                    (cons 'N
                          (↦ (i5 (set 'N)) (x5 (set 2)) (y5 (set 13))))
                    (cons 'N
                          (↦ (i6 (set 'N)) (x6 (set 3)) (y6 (set 13))))
                    (cons 'N
                          (↦ (i7 (set 'N)) (x7 (set 7)) (y7 (set 13))))))

  (check-diverge (eval (fact 5)))
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push))
  
  (check-match (eval ref-sref)
               (set (cons 'failure (↦ (x (set 'N))))
                    (cons 42       (↦ (y (set 'N)))))))
  