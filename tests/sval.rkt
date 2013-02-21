#lang racket
(require rackunit
         "../sval.rkt"
         "../syntax.rkt"
         "util.rkt")

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v)
     (check-match (eval e)
                  (cons v σ))]
    [(check-eval e v0 v1)
     (check-match (eval e)
                  (both-ans (cons v0 σ1)
                            (cons v1 σ2)))]))

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
(check-eval (ref (num 5))
            (? symbol?))
(check-eval (drf (ref (num 5))) 5)
(check-eval (drf (srf (ref (num 5)) (num 7))) 7)
(check-eval (op1 'add1
                 (ifz (drf (srf (ref (num 0)) (num 1)))
                      'err
                      (num 42)))
            43)
(check-eval (op1 'add1
                 (ifz (drf (srf (ref (num 1)) (num 0)))
                      'err
                      (num 42)))
            'err)

(check-eval (op1 'add1 (ifz (sym 'n) (num 7) (num 8)))
            8
            9)
            
(check-eval (op1 'add1 (app (sym 'f) (num '7)))
            '(add1 (f 7)))
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