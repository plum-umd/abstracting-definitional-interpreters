#lang racket
;; Finite map data structure
(require racket/hash)
(provide ∅ ∈ ⊔ map-to-hash size)

(define (map-print m port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (recur (hash->list (map-to-hash m)) port)))

(struct map (to-hash)
  #:transparent
  #:property
  prop:procedure
  (case-lambda
    [(m x) 
     (hash-ref (map-to-hash m) x)]
    [(m x v)
     (map (hash-set (map-to-hash m) x v))])
  #:methods gen:custom-write
  [(define write-proc map-print)])

(define ∅ (map (hash)))

(define (size m) (hash-count (map-to-hash m)))

(define (∈ k m) (hash-has-key? (map-to-hash m) k))

(define (⊔ m₁ m₂ #:combine [cod-⊔ set-union])
  (map (hash-union (map-to-hash m₁)
                   (map-to-hash m₂)
                   #:combine cod-⊔)))

(module+ test
  (require rackunit)
  (define r ∅)
  (check-equal? ((r 'x 1) 'x) 1)
  (check-equal? ((r 'x 1) 'y 2) ((r 'y 2) 'x 1)))
