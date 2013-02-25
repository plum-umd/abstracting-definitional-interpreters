#lang racket
(require rackunit
	 "../set.rkt")


(check-equal? (set 1 2 3)
	      (set 3 2 1))

(check-true (match (set 1 2 3)
              [(set 3 2 1) #true]))

(check-true (match (set 1 2 3)
              [(set 3 2 x ...) #true]))

(check-equal? (match (set 1 2 3)
                [(set 3 2 x ...) x])
	      (set 1))




