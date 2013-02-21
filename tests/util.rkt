#lang racket
(provide set)
(require rackunit)

(define-match-expander set
  (syntax-rules ()
    [(set p ...)
     (? set? (app set->list (list-no-order p ...)))])
  (make-rename-transformer #'set))
