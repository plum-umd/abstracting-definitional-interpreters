#lang racket/base
(provide fix)

;(: fix (∀ (X Y) ((X → Y) → X → Y) → X → Y))
(define ((fix f) x) ((f (fix f)) x))

(module+ test
  (require rackunit)
  ;(: fact : (Integer → Integer) → Integer → Integer)
  (define ((fact rec) n)
    (if (zero? n) 1 (* n (rec (- n 1)))))

  (check-equal? (* 5 4 3 2 1) ((fix fact) 5)))
