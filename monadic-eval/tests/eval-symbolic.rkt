#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../set.rkt"
           "../fixed/eval-symbolic.rkt"
           "../parser.rkt"
           "tests.rkt")
  
  (check-match (eval (dd ''n))
               (set (cons (cons  2 (↦ (i1 'n) (x1 2) (y1 11))) (set 'n))
                    (cons (cons 13 (↦ (i0 'n) (x0 7) (y0 13))) (set '(¬ n)))))

  (check-match (eval (dd 0))
               (set (cons (cons 2 (↦ (i 0) (x 2) (y 11))) (set))))

  (check-match (eval (dd 1))
               (set (cons (cons 13 (↦ (i 1) (x 7) (y 13))) (set))))

  (check-match (eval (dd* ''n))
               (set (cons (cons 91 (↦ (i0 'n) (x0 7) (y0 13))) (set '(¬ n)))
                    (cons (cons 22 (↦ (i1 'n) (x1 2) (y1 11))) (set 'n))))
  
  (check-match (eval (dd* 0))
               (set (cons (cons 22 (↦ (i 0) (x 2) (y 11))) (set))))

  (check-match (eval (dd* 1))
               (set (cons (cons 91 (↦ (i 1) (x 7) (y 13))) (set))))
 
  (check-match (eval (fact 5))
               (set (cons (cons 120 (↦ (x0 5) (x1 4) (x2 3) (x3 2) (x4 1) (x5 0) (f _))) (set))))  
  
  (check-diverge (eval (fact ''n))) 
  (check-diverge (eval (fact -1)))
  (check-diverge (eval omega))
  (check-diverge (eval omega-push))
  
  (check-match (eval ref-sref)
               (set (cons (cons 42 (↦ (_ 0))) (set))))

  (define fail_42 (parse `(if0 's (quotient 1 's) 42)))
  (check-match (eval fail_42)
               (set (cons (cons 42       (↦)) (set '(¬ s)))
                    (cons (cons 'failure (↦)) (set 's))))
  
  (define 1/s_42 (parse `(if0 (¬ 's) (quotient 1 's) 42)))
  (check-match (eval 1/s_42)
               (set (cons (cons 42 (↦)) (set 's))
                    (cons (cons '(quotient 1 s) (↦)) (set '(¬ s))))))
