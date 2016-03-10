#lang racket
;; Finite map data structure
(provide ∅)

(define (map-print m port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (recur (hash->list (map-t m)) port)))

(struct map (t)
  #:transparent
  #:property
  prop:procedure
  (case-lambda
    [(m x) 
     (hash-ref (map-t m) x)]
    [(m x v)
     (map (hash-set (map-t m) x v))])
  #:methods gen:custom-write
  [(define write-proc map-print)])

(define ∅ (map (hash)))

(module+ test
  (require rackunit)
  (define r ∅)
  (check-equal? ((r 'x 1) 'x) 1)
  (check-equal? ((r 'x 1) 'y 2) ((r 'y 2) 'x 1)))