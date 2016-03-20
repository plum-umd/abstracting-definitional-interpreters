#lang racket
(require "syntax.rkt"
         "set.rkt"
         "map.rkt"
         "fv.rkt")
(provide (all-defined-out))

(define ((gc as) σ)
  (restrict σ (reachable as σ)))

(define (reachable as s)
  (define (R to-see seen)
    (match to-see
      [(set) seen]
      [(set a as ...)
       (R (set-subtract (set-union (roots-v (s a)) as) seen)
          (set-add seen a))]))
  (R as (set)))

(define (roots e ρ)
  (for/set ([x (in-set (fv e))])
    (ρ x)))

(define (roots-v v)
  (match v
    [(cons l ρ) (roots l ρ)]
    [_ (set)]))

  

          
