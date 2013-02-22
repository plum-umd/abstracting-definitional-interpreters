#lang racket
(require "syntax.rkt" "parser.rkt" "units.rkt")
(require (for-syntax racket/syntax))

(require racket/stxparam)

(define-syntax-parameter linkage (syntax-rules ()))

(provide #%datum
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(define run (box #f)) ;; holds the evaluator once the module is run.

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . e)
     #`(#%top-interaction . ((unbox run) 'e))]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ l e ...)
     (begin
      (define linkage (syntax->datum #'l))
       #`(#%module-begin
	  (define-values/invoke-unit/infer #,linkage)
          (set-box! run (λ (x) (eval (parse x))))
          ((unbox run) 'e) ...))]))
