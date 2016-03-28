#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-pdcfa.rkt"
           (rename-in "../fixed/eval-apply-pdcfa.rkt" [eval eval-apply])
           "tests.rkt")
  
  (define-syntax-rule (check-eval eval)
    (begin
      ;; only testing soundness for tricky012
      (check-match (eval (tricky012 '(sub1 1)))
                   (cons (set (cons 0 _) _ (... ...)) cache))
      (check-match (eval (tricky012 '(add1 0)))
                   (cons (set (cons 1 _) _ (... ...)) cache))
      (check-match (eval (tricky012 '(add1 1)))
                   (cons (set (cons 2 _) _ (... ...)) cache))

      (check-match (eval (dd* '(add1 0)))
                   (cons (set (cons 'N (↦ (i0 (set 'N)) (x0 (set 2)) (y0 (set 11))))
                              (cons 'N (↦ (i1 (set 'N)) (x1 (set 3)) (y1 (set 11))))
                              (cons 'N (↦ (i2 (set 'N)) (x2 (set 5)) (y2 (set 11))))
                              (cons 'N (↦ (i3 (set 'N)) (x3 (set 7)) (y3 (set 11))))
                              (cons 'N (↦ (i4 (set 'N)) (x4 (set 2)) (y4 (set 13))))
                              (cons 'N (↦ (i5 (set 'N)) (x5 (set 3)) (y5 (set 13))))
                              (cons 'N (↦ (i6 (set 'N)) (x6 (set 5)) (y6 (set 13))))
                              (cons 'N (↦ (i7 (set 'N)) (x7 (set 7)) (y7 (set 13)))))
                         cache))
      
      (check-match (eval (dd* 0))
                   (cons (set (cons 'N (↦ (i (set 0)) (x (set 2)) (y (set 11)))))
                         cache))
      
      (check-match (eval (dd* 1))
                   (cons (set (cons 'N (↦ (i (set 1)) (x (set 7)) (y (set 13)))))
                         cache))
      
      (check-match (eval (dd '(add1 0)))
                   (cons (set (cons  5 (↦ (i0  (set 'N)) (x0  (set 5)) (y0  (set 13))))
                              (cons 11 (↦ (i1  (set 'N)) (x1  (set 3)) (y1  (set 11))))
                              (cons  2 (↦ (i2  (set 'N)) (x2  (set 2)) (y2  (set 13))))
                              (cons 11 (↦ (i3  (set 'N)) (x3  (set 5)) (y3  (set 11))))
                              (cons  7 (↦ (i4  (set 'N)) (x4  (set 7)) (y4  (set 13))))
                              (cons  5 (↦ (i5  (set 'N)) (x5  (set 5)) (y5  (set 11))))
                              (cons  3 (↦ (i6  (set 'N)) (x6  (set 3)) (y6  (set 13))))
                              (cons  7 (↦ (i7  (set 'N)) (x7  (set 7)) (y7  (set 11))))
                              (cons 11 (↦ (i8  (set 'N)) (x8  (set 7)) (y8  (set 11))))
                              (cons  3 (↦ (i9  (set 'N)) (x9  (set 3)) (y9  (set 11))))
                              (cons 11 (↦ (i10 (set 'N)) (x10 (set 2)) (y10 (set 11))))
                              (cons  2 (↦ (i11 (set 'N)) (x11 (set 2)) (y11 (set 11))))
                              (cons 13 (↦ (i12 (set 'N)) (x12 (set 5)) (y12 (set 13))))
                              (cons 13 (↦ (i13 (set 'N)) (x13 (set 2)) (y13 (set 13))))
                              (cons 13 (↦ (i14 (set 'N)) (x14 (set 3)) (y14 (set 13))))
                              (cons 13 (↦ (i15 (set 'N)) (x15 (set 7)) (y15 (set 13)))))
                         cache))
      
      (check-match (eval (dd 0))
                   (cons (set (cons 2 (↦ (i (set 0)) (x (set 2)) (y (set 11)))))
                         cache))
      
      (check-match (eval (dd 1))
                   (cons (set (cons 13 (↦ (i (set 1)) (x (set 7)) (y (set 13)))))
                         cache))
      
      (check-match (eval (fact 5))
                   (cons (set (cons 'N (↦ (x (set 'N 5)) (f _))))
                         cache))
      
      (check-match (eval (fact -1))
                   (cons (set (cons 'N (↦ (x (set 'N -1)) (f _))))
                         cache))
      
      (check-match (eval omega)
                   (cons (set) cache))
      
      (check-match (eval omega-push)
                   (cons (set) cache))))

  ;; the eval-apply version doesn't have references,
  ;; so test eval-only version separately.
  (check-match (eval ref-sref)
               (cons (set (cons 42       (↦ (_ (set 0 1))))
                          (cons 'failure (↦ (_ (set 0 1)))))
                     cache))

  (check-eval eval)
  (check-eval eval-apply))
  