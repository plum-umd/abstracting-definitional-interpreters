#lang racket
(require "../syntax.rkt"
         "../parser.rkt"
         "../evals/eval-trace.rkt")

(require (for-syntax racket/syntax))

(provide #%datum
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . e)
     #;(eval (parse 'e))
     #`(#%top-interaction . (eval (parse 'e)))]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ e ...)
     #`(#%module-begin
        (eval (parse 'e)) ...)]))

