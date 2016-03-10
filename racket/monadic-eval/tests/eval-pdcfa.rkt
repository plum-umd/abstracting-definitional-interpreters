#lang racket
(require rackunit
         "../fix.rkt"
         "../map.rkt"
         "../set.rkt"
         "../transformers.rkt"
         "../units.rkt"
         "../syntax.rkt")

(define-values/invoke-unit/infer
  (link monad-pdcfa@ state-nd@ alloc-0cfa@ δ-abs@ ev!@ ev-cache@ eval-coind@ ev-debug@))

(define-monad M)

(define (eval e)
  ;(mrun ((eval-coind (fix (ev-debug (ev-cache ev!)))) e))
  (mrun ((eval-coind (fix (ev-cache ev!))) e)))

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v ...)
     (check-match (caar (eval e)) (set v ...))]))

(module+ test
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
  (check-eval (ifz (op1 'add1 (num 0)) (num 7) (num 8))
              8 7)
  
  (check-eval (ref (num 5))
              (cons 'box _))
  (check-eval (drf (ref (num 5))) 5)
  (check-eval (drf (srf (ref (num 5)) (num 7))) 5 7)
  (check-eval (op1 'add1
                   (ifz (drf (srf (ref (num 0)) (num 1)))
                        'err
                        (num 42)))
              'N)
  (check-eval (op1 'add1
                   (ifz (drf (srf (ref (num 1)) (num 0)))
                        'err
                        (num 42)))
              'N)
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
  (check-eval (app (lam 'x (app (vbl 'x) (vbl 'x)))
                   (lam 'x (app (vbl 'x) (vbl 'x)))))
  (check-eval (app (lam 'x (app (vbl 'x) (app (vbl 'x) (vbl 'x))))
                   (lam 'x (app (vbl 'x) (app (vbl 'x) (vbl 'x))))))
  )
