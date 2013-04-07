#lang racket
(provide (all-defined-out))
(require "set.rkt")
(require "store.rkt")
(require "syntax.rkt")

(define (clean e r s)
  (list e r (gc e r s)))

(define (gc e r s)
  (restrict s
            (reachable (rng (restrict r (fv e))) s)))

(define (reachable as s)
  (define (R to-see seen)
    (match to-see
      [(set) seen]
      [(set a as ...)
       (R (set-subtract (set-union (touch a s) as) seen)
          (set-add seen a))]))
  (R as (set)))

(define (touch a s)
  (define vs (hash-ref s a))
  (for/fold ([as (set)])
    ([v (in-set vs)])
    (match v
      [(cons e r) 
       (set-union as
                  (rng (restrict r (fv e))))]
      [_ as])))

(define (rng r)
  (for/set ([a (in-hash-values r)])
           a))

(define (restrict r xs)
  (for/hash ([x (in-hash-keys r)]
             #:when (set-member? xs x))
    (values x (hash-ref r x))))

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
     (set-remove (set-union (fv e0) (fv e1)) f)]))


          