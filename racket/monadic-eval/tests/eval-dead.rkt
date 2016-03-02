#lang racket
(require rackunit
	 "../syntax.rkt"
         "../evals/eval-dead.rkt"
	 "util.rkt")

(define-syntax check-eval
  (syntax-rules ()
    [(check-eval e v [σ ...] d ...)
     (check-match (eval e)
                  (cons (cons v (hash-table σ ...))
                        (set d ...)))]))

(check-eval '(num 5) 5 ())
(check-eval '(ifz (num 0) (num 2) (num 3)) 2 () (num 3))
