#lang racket
;; Finite map data structure
(require racket/hash)
(provide ∅ ⊔ ∈)

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

(define-syntax ∈
  (syntax-rules ()
    [(∈ k m) (hash-has-key? (map-to-hash m) k)]
    [(∈ m)   (in-hash (map-to-hash m))]))

(define ∅ (map (hash)))

(define (⊔ m₁ m₂ #:combine [cod-⊔ (λ (x _) x)])
  (map (hash-union (map-to-hash m₁)
                   (map-to-hash m₂)
                   #:combine cod-⊔)))

(module+ test
  (require rackunit)
  (define r ∅)
  (check-equal? ((r 'x 1) 'x) 1)
  (check-equal? ((r 'x 1) 'y 2) ((r 'y 2) 'x 1))
  (check-equal? (∈ 'x (r 'x 1)) #t)
  (check-equal? ('y . ∈ . (r 'x 1)) #f)
  (check-true
   (let ([l (for/list ([(k v) (∈ ((r 'x 1) 'y 2))])
              (cons k v))])
     (not (false? (and (member (cons 'y 2) l) (member (cons 'x 1) l)))))))
