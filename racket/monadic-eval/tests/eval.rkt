#lang racket
(require rackunit
         "../transformers.rkt"
         "../fix.rkt"
         "../syntax.rkt"
         "../units.rkt")

(define-values/invoke-unit/infer
  (link monad@ alloc@ δ@ ev@))

(define (eval e) (mrun ((fix ev) e)))

(define-syntax-rule (check-eval e v)
  (check-match (eval e) (cons v _)))

(define-syntax-rule (check-fail e)
  (check-match (eval e) (failure)))

(module+ test
  (check-eval (num 5) 5)
  (check-eval (op1 'add1 (num 5)) 6)
  (check-eval (op2 '+ (num 5) (num 11)) 16)
  (check-eval (lam 'x (vbl 'x))
              (cons (lam 'x (vbl 'x)) ρ))

  (check-eval (app (lam 'x (num 7)) (num 5)) 7)
  (check-eval (app (lam 'x (lam '_ (vbl 'x))) (num 5))
              (cons (lam '_ (vbl 'x)) ρ))
  (check-eval (app (lam 'x (vbl 'x)) (num 5)) 5)

  (check-eval (ifz (num 0) (num 7) (num 8)) 7)
  (check-eval (ifz (num 1) (num 7) (num 8)) 8)
  (check-eval (lrc 'f (lam 'x
                           (ifz (vbl 'x)
                                (vbl 'x)
                                (op1 'add1
                                     (app (vbl 'f)
                                          (op2 '+
                                               (vbl 'x)
                                               (num -1))))))
                   (app (vbl 'f)
                        (num 5)))
              5)
  (check-eval (op2 'quotient (num 1) (num 3))
              0)
  (check-fail (op2 'quotient (num 1) (num 0))))
