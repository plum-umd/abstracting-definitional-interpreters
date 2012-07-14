#lang racket
(provide set check-match)
(require rackunit)

(define-match-expander set
  (syntax-rules ()
    [(set p ...)
     (? set? (app set->list (list-no-order p ...)))])
  (make-rename-transformer #'set))

(define-syntax check-match
  (syntax-rules ()
    [(check-match e p)
     (check-true (match e
                   [p #true]
                   [_ #false]))]))