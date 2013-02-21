#lang racket
(require rackunit
         "../pdcfa.rkt"
         "../syntax.rkt"
         "util.rkt")

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v ...)
     (check-match (eval e)
                  (set (cons v _) ...))]))

(check-eval (num 5) 5)
(check-eval (op1 'add1 (num 5)) 'N)
(check-eval (op2 '+ (num 5) (num 11)) 'N)
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
(check-eval (drf (srf (ref (num 5)) (num 7))) 5 7)
(check-eval (op1 'add1
                 (ifz (drf (srf (ref (num 0)) (num 1)))
                      'fail
                      (num 42)))
            'N
            'fail)
(check-eval (op1 'add1
                 (ifz (drf (srf (ref (num 1)) (num 0)))
                      'fail
                      (num 42)))
            'N
            'fail)

(define Ω
  (app (lam 'x (app (vbl 'x) (vbl 'x)))
       (lam 'y (app (vbl 'y) (vbl 'y)))))


(check-match (eval Ω) (set))

(check-eval (ifz (op1 'add1 (num 7))
                 (num 7)
                 (num 8))
            7
            8)
(check-eval (op1 'add1
                 (ifz (op1 'add1 (num 7))
                      (num 7)
                      (num 8)))
            'N)

(check-eval (app (sym 'f) (num 5))
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
            'N)

(check-eval (lrc 'f (lam 'x
                         (ifz (vbl 'x)
                              (vbl 'x)
                              (app (vbl 'f)
                                   (op1 'add1
                                        (app (vbl 'f)
                                             (vbl 'x))))))
                 (app (vbl 'f)
                      (sym 'N)))
            'N)

;; Is this a soundness bug?
(check-eval (lrc 'f (lam 'x
                         (ifz (vbl 'x)
                              (num 0)
                              (app (vbl 'f)
                                   (sym 'N))))
                 (app (vbl 'f)
                      (sym 'N)))
            0)

;; This used to be a soundness bug producing {0} on
;; first iteration.  Fixed with fixed points!
(check-eval (lrc 'f (lam 'x
                         (ifz (vbl 'x)
                              (num 0)
                              (ifz (app (vbl 'f)
                                        (op1 'sub1 (sym 'x)))
                                   (num 7)
                                   (num 9))))
                 (app (vbl 'f)
                      (op1 'sub1 (num 2))))
            0 7 9)
                

(check-eval (ifz (sym 'N)
                 (num 5)
                 (num 7))
            5 7)

(check-eval (ifz (sym 'N)
                 (num 5)
                 Ω)
            5)
