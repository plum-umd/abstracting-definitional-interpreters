#lang racket
(require rackunit
         (prefix-in cyc: "../evals/eval-pure.rkt")
         (prefix-in box: "../evals/eval-box.rkt")
         (prefix-in sto: "../evals/eval-pure-explicit.rkt")
         "../syntax.rkt"
         "util.rkt")

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v)
     (begin
       (check-match (cyc:eval e) v)
       (check-match (box:eval e) v)
       (check-match (sto:eval e) (cons v _)))]))

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
