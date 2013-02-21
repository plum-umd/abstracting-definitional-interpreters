#lang racket
(require "syntax.rkt" "parser.rkt")
(require (for-syntax racket/syntax))

(provide #%datum #%top-interaction (rename-out [my-module-begin #%module-begin]))

#;(define-syntax #%top-interaction (make-#%top-interaction #'-->v typable?))
(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ l e ...)
     (begin
      (define lang (format-id #f "monadic-eval/~a" (syntax->datum #'l)))
       #`(#%module-begin
          (require #,lang)
          (eval (parse 'e)) ...))]))
