#lang racket
(require rackunit
         racket/match
         racket/set
         "../syntax.rkt"
         "../evals/eval-dead.rkt")

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v s)
     (check-equal? (car (eval e))
                   (cons v s))]))
(module+ test
  (check-eval '(num 5) 5 {set})
  (check-eval '(ifz (num 0) (num 2) (num 3)) 2 {set (num 3)})
  
  (check-eval (lrc 'f (lam 'x
                           (ifz (vbl 'x)
                                (vbl 'x)
                                (op1 'add1
                                     (app (vbl 'f)
                                          (op2 '+
                                               (vbl 'x)
                                               (ifz (num 1)
                                                    'err
                                                    (num -1)))))))
                   (app (vbl 'f)
                        (num 5)))
              5
              {set 'err}))
