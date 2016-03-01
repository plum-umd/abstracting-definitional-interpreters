#lang racket
(provide eval)
(require "../fix.rkt"
         "../units/pdcfa-unit.rkt"
         "../units/ev-symbolic-unit.rkt"
         "../units/delta-unit.rkt"
         "../units/sto-0cfa-unit.rkt")

(define-values/invoke-unit/infer
  (link pdcfa@ ev-symbolic@ abs-δ@ sto-0cfa@))

;; FIXME: use fix

;; eval : E ->_total [Setof Ans]
;; iterates ev until reaching a fixed point in the memo-table

(define (eval e)
  
  (define ((((ev′ e ρ) σ) m1) m2)
    (define ers (list e ρ σ))
    (define anss (hash-ref m1 ers #false))  
    (if anss
        (cons anss m1)
        (match (((((ev e ρ) ev′) σ)
                 (hash-set m1 ers (hash-ref m2 ers (set)))) m2)                
          [(cons anss m1)
           (cons anss (hash-set m1 ers anss))])))
  
  
  (let loop ([m2 (hash)] [anss (set)])
    (match ((((ev′ e (hash)) (hash)) (hash)) m2)
      [(and r (cons anss1 m1))
       (if (equal? r (cons anss m2))
	   anss1
	   (loop m1 anss1))])))

