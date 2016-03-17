#lang racket
(provide (all-defined-out))
(require "set.rkt"
         "map.rkt"
         "syntax.rkt")

(define (clean e r s)
  (list e r (gc e r s)))

(define (gc e r s)
  (restrict s (live e r s)))

(define (live e ρ σ)
  (reachable (rng (restrict ρ (fv e))) σ))

(define (reachable as s)
  (define (R to-see seen)
    (match to-see
      [(set) seen]
      [(set a as ...)
       (R (set-subtract (set-union (touch a s) as) seen)
          (set-add seen a))]))
  (R as (set)))

(define (touch a s)
  (define vs (s a))
  (for/fold ([as (set)])
    ([v (in-set vs)])
    (match v
      [(cons 'box a)
       (set-add (s a) a)]
      [(cons e r) 
       (set-union as
                  (rng (restrict r (fv e))))]
      [_ as])))

(define (rng r)
  (for/set ([(_ v) (∈ r)]) v))

(define (restrict r xs)
  (for/map ([(x _) (∈ r)]
            #:when (set-member? xs x))
    (values x (r x))))

(define (map-remove m x)
  (for/map ([(y v) (∈ m)]
            #:when (not (equal? x y)))
    (values y v)))

(define (fv e)
  (match e
    [(app e0 e1) (set-union (fv e0) (fv e1))]
    [(lam x e) (set-remove (fv e) x)]
    [(vbl x) (set x)]
    [(num n) (set)]
    [(ifz e0 e1 e2)
     (set-union (fv e0)
                (fv e1)
                (fv e2))]
    [(op1 o e)
     (fv e)]
    [(op2 o e0 e1)
     (set-union (fv e0)
                (fv e1))]
    [(ref e)
     (fv e)]
    [(drf e)
     (fv e)]
    [(srf e0 e1)
     (set-union (fv e0) (fv e1))]
    [(sym s) (set)]
    [(lrc f e0 e1)
     (set-remove (set-union (fv e0) (fv e1)) f)]
    [_ (set)]))

(define (count-fv e)
  (match e
    [(app e0 e1) (⊔ (count-fv e0) (count-fv e1) #:combine +)]
    [(lam x e)   (map-remove (count-fv e) x)]
    [(vbl x) (∅ x 1)]
    [(num n) ∅]
    [(ifz e0 e1 e2)
     (⊔ (count-fv e0)
        (count-fv e1)
        (count-fv e2)
        #:combine +)]
    [(op1 o e)
     (count-fv e)]
    [(op2 o e0 e1)
     (⊔ (count-fv e0)
        (count-fv e1)
        #:combine +)]
    [(ref e)
     (count-fv e)]
    [(drf e)
     (count-fv e)]
    [(srf e0 e1)
     (⊔ (count-fv e0) (count-fv e1) #:combine +)]
    [(sym s) ∅]
    [(lrc f e0 e1)
     (map-remove (⊔ (count-fv e0) (count-fv e1) #:combine +)
                 f)]
    [_ ∅]))


          
