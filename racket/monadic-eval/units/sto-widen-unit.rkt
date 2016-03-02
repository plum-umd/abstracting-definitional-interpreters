#lang racket/unit
(require racket/match
         "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt"
         "../set.rkt")
(import monad^ return-vals^ return-ans^)
(export env^ sto^)

(define ext hash-set) ; ?

(define ((get r x) s)
  ((return-vals (lookup s r x)) s))

(define ((alloc f v) s)
  (match f
    [(cons (lam x e) r)
     (define a x) ; 0CFA-like abstraction
     ;; widen on sto update
     (return-ans a
       (update-sto s a 
         (widen (hash-ref s a (set)) v)))]))

(define ((ralloc x v) s)
  (match v
    [(cons e r)
     (define a x)
     ((return a) 
      (join-sto s a (cons e (hash-set r x a))))]))

(define (widen s v)
  (match v
    [(? number?)
     (match s
       [(set) (set v)]
       [(set 'N _ ...) s]
       [(set (? (λ (m) (equal? m v))) _ ...) s]
       [(set (? number? m) _ ...)
        (join-numeric s)]
       [_
        (set-add s v)])]
    ['N
     (join-numeric s)]
    [x
     (set-add s x)]))




(define ((new v) s)
  (define a 'box) ; One box per program abstraction
  (return-ans a (join-sto s a v)))

(define ((sbox a v) s)
  (return-ans a (join-sto s a v)))

(define ((ubox a) s)
  ((return-vals (lookup-sto s a)) s))

(define (join-numeric s)
  (set-add (for/set ([x (in-set s)]
                     #:unless (number? x))
                    x)
           'N))
