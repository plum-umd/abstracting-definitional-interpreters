#lang racket
(require rackunit
         "../eval-bang.rkt"
         "../syntax.rkt"
         "../util.rkt")

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v)
     (check-match (eval e) v)]))

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
            (? box?))
(check-eval (ubx (ref (num 5))) 5)
(check-eval (ubx (sbx (ref (num 5)) (num 7))) 7)
(check-eval (op1 'add1
                 (ifz (ubx (sbx (ref (num 0)) (num 1)))
                      'fail
                      (num 42)))
            43)
(check-eval (op1 'add1
                 (ifz (ubx (sbx (ref (num 1)) (num 0)))
                      'fail
                      (num 42)))
            'fail)
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

