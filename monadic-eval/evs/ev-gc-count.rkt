#lang racket/unit
(require racket/match
         racket/set
         "../map.rkt"
         "../transformers.rkt"
         "../signatures.rkt"
         "../syntax.rkt")
(import monad^ gc-count^ state^ alloc^ menv^ mstore^ mlive^)
(export ev-gc-count^)
(init-depend monad^)

(define-monad M)

(define (((ev-count ev0) ev) e)
  (define mₑ
    (match e
      [(ref e₀) (do (and (cons 'box a) v) ← ((ev0 ev) e)
                    (update-live
                      (λ (α)
                        (if (∈ a α)
                            (α a (add1 (α a)))
                            (α a 1))))
                    (return v))]
      [(drf e₀) (do (cons 'box a) ← (ev e₀)
                    (update-live
                      (λ (α) (α a (sub1 (α a)))))
                    v ← (find a)
                    (return v))]
      [_        (do v ← ((ev0 ev) e)
                    (return v))]))
  (count e mₑ))

(define (((ev-gc ev0) ev) e)
  (do v ← ((ev0 ev) e)
      (gc e)
      (return v)))

