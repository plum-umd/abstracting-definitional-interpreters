#lang racket
(require "syntax.rkt" "parser.rkt" "units.rkt")
(require (for-syntax racket/syntax))

(provide #%datum #%top-interaction (rename-out [my-module-begin #%module-begin]))

#;(define-syntax #%top-interaction (make-#%top-interaction #'-->v typable?))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ l e ...)
     (begin
      (define linkage (syntax->datum #'l))
       #`(#%module-begin
	  (define-values/invoke-unit/infer #,linkage)
          (eval (parse 'e)) ...))]))
