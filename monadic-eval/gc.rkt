#lang racket
(require "syntax.rkt"
         "set.rkt"
         "map.rkt"
         "fv.rkt")
(provide (all-defined-out))

(define ((gc as) σ)
  (restrict σ (reachable as σ)))

;; Works on both set-based and value-based stores.
(define (reachable as s)
  (define (R to-see seen)
    (match to-see
      [(set) seen]
      [(set a as ...)
       (define sa (s a))
       (define as* (if (set? sa)
                       (for/fold ([as* (set)])
                                 ([v (in-set sa)])
                         (set-union as* (roots-v v)))                   
                       (roots-v sa)))
       (R (set-subtract (set-union as* as) seen)
          (set-add seen a))]))
  (R as (set)))

(define (roots e ρ)
  (for/set ([x (in-set (fv e))])
    (ρ x)))

(define (roots-v v)
  (match v
    [(cons l ρ) (roots l ρ)]
    [_ (set)]))

  

          
