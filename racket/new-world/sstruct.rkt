#lang racket
(provide struct)

(define-syntax struct
  (syntax-rules ()
    [(struct name (fld ...) #:symbolic)
     (define-match-expander name
       (syntax-rules ()
         [(_ fld ...)
          (list 'name fld ...)])
       (syntax-id-rules ()
         [(name fld ...) (list 'name fld ...)]
         [_ (λ (fld ...) (list 'name fld ...))]))]))
