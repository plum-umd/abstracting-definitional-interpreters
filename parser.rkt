#lang racket
(require "syntax.rkt")
(provide parse)

(define (parse e)
  (match e
    ['err 'err]
    [(? number? n) (num n)]
    [(? symbol? s) (vbl s)]
    [(list 'λ (list x) e)
     (lam x (parse e))]
    [(list 'let x e0 e1)
     (app (lam x (parse e1)) (parse e0))]
    [(list 'if0 e0 e1 e2)
     (ifz (parse e0)
          (parse e1)
          (parse e2))]
    [(list '¬ e)
     (op1 '¬ (parse e))]
    [(list 'add1 e)
     (op1 'add1 (parse e))]
    [(list 'sub1 e)
     (op1 'sub1 (parse e))]
    [(list '+ e0 e1)
     (op2 '+ (parse e0) (parse e1))]
    [(list '- e0 e1)
     (op2 '- (parse e0) (parse e1))]
    [(list '* e0 e1)
     (op2 '* (parse e0) (parse e1))]
    [(list 'quotient e0 e1)
     (op2 'quotient (parse e0) (parse e1))]
    [(list 'ref e)
     (ref (parse e))]
    [(list '! e)
     (drf (parse e))]
    [(list e0 ':= e1)
     (srf (parse e0) (parse e1))]
    [(list 'quote s)
     (sym s)]
    [(list 'rec f e0 e1)
     (lrc f (parse e0) (parse e1))]
    [(list e0 e1)
     (app (parse e0) (parse e1))]))
