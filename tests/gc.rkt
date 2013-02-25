#lang racket
(require rackunit
         "../gc.rkt"
         "../syntax.rkt")

(check-equal? (gc (num 1) (hash 'x 'x) (hash 'x (set 1)))
              (hash))
(check-equal? (gc (vbl 'x) (hash 'x 'x) (hash 'x (set 1)))
              (hash 'x (set 1)))
(check-equal? (gc (lam 'x (vbl 'y))
                  (hash 'z 'z
                        'y 'y)
                  (hash 'z (set 1)
                        'y (set 2)))
              (hash 'y (set 2)))