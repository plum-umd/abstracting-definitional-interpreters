#lang racket
(require "syntax.rkt")
(provide fv)

(define (fv e)
  (match e
    [(app e0 e1) (set-union (fv e0) (fv e1))]
    [(lam x e) (set-remove (fv e) x)]
    [(vbl x) (set x)]
    [(num n) (set)]
    [(ifz e0 e1 e2)
     (set-union (fv e0)
                (fv e1)
                (fv e2))]
    [(op1 o e)
     (fv e)]
    [(op2 o e0 e1)
     (set-union (fv e0)
                (fv e1))]
    [(ref e)
     (fv e)]
    [(drf e)
     (fv e)]
    [(srf e0 e1)
     (set-union (fv e0) (fv e1))]
    [(sym s) (set)]
    [(lrc f e0 e1)
     (set-remove (set-union (fv e0) (fv e1)) f)]
    [_ (set)]))
