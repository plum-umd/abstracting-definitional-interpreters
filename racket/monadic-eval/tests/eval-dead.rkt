#lang racket
(require rackunit
         racket/match
         racket/set
         "../transformers.rkt"
         "../fix.rkt"
         "../syntax.rkt"
         "../units.rkt")

(define-values/invoke-unit/infer
  (link ev-base@ ev-dead@ eval-dead@ monad-dead@ alloc-nat@
        delta-con@ ref-explicit@ st-explicit@))

(define (eval e) (mrun ((eval-dead (fix (ev-dead ev))) e)))

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v s)
     (check-equal? (let ([out (eval e)]) (cons (caar out) (cdr out)))
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
