#lang racket/base
(provide fix fix2)

;(: fix (∀ (X Y) ((X → Y) → X → Y) → X → Y))
(define ((fix f) x) ((f (fix f)) x))

(define ((fix2 f g) x) ((f (fix2 f g) (fix2 g f)) x))

(module+ test
  (require rackunit)
  ;(: fact : (Integer → Integer) → Integer → Integer)
  (define ((fact rec) n)
    (if (zero? n) 1 (* n (rec (- n 1)))))

  (check-equal? (* 5 4 3 2 1) ((fix fact) 5))

  (define even?
    (fix2 (λ (e? o?) (λ (x) (or (zero? x) (o? (sub1 x)))))
          (λ (o? e?) (λ (x) (and (not (zero? x)) (e? (sub1 x)))))))

  (check-equal? (even? 5) #false)
  (check-equal? (even? 6) #true))
