#lang racket
(provide subexps)
(require "syntax.rkt")

(define (subexps e)
  (set-add (match e
             [(or 'err
                  (vbl _)
                  (sym _)
                  (num _))
              (set)]
             [(or (op1 _ e)
                  (ref e)
                  (drf e)
                  (lam _ e))
              (subexps e)]
             [(or (op2 _ e0 e1)
                  (srf e0 e1)
                  (lrc _ e0 e1)
                  (app e0 e1))
              (set-union (subexps e0)
                         (subexps e1))]
             [(ifz e0 e1 e2)
              (set-union (subexps e0)
                         (subexps e1)
                         (subexps e2))])
           e))