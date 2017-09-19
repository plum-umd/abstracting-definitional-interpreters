#lang racket
(require rackunit
         racket/engine
         "../parser.rkt")
(provide tricky012 dd dd* fact omega omega-push ref-sref check-diverge runs-for?)

(define-syntax-rule
  (check-diverge e)
  (check-true (runs-for? 3 e)))

(define-syntax-rule
  (runs-for? seconds e)
  (not (engine-run (* 1000 seconds) (engine (λ (_) e)))))

;; identity function on {0..2}, but triggers soundness bug
;; in pushdown approaches that don't iterate to fixed-point properly.
(define (tricky012 N)
  (parse
   `((rec f (λ (x)
             (if0 x 0 (if0 (f (sub1 x)) 1 2))))
      ,N)))

(define (dd N)
  ;; returns 2 if N=0, 13 otherwise
  (parse
   `(let ([input ,N])
      (let ([x (if0 input (if0 input 2 3) (if0 input 5 7))])
        (let ([y (if0 input 11 13)])
          (if0 input x y))))))

(define (dd* N)
  ;; returns 22 if N=0, 91 otherwise
  (parse
   `(let ([input ,N])
      (let ([x (if0 input (if0 input 2 3) (if0 input 5 7))])
        (let ([y (if0 input 11 13)])
          (* x y))))))

(define (fact N)
  ;; DIVERGES when N < 0
  (parse
   `((rec f (λ (x)
             (if0 x
                  1
                  (* x (f (- x 1))))))
         ,N)))

(define omega
  ;; DIVERGES
  (parse `(let ([U (λ (f) (f f))])
            (U U))))

(define omega-push
  ;; DIVERGES and continually pushes on new stack frames
  (parse `(let ([Uₚ (λ (f) (f (f f)))])
            (Uₚ Uₚ))))

(define ref-sref
  (parse '(if0 (! ((ref 1) := 0)) 42 err)))
