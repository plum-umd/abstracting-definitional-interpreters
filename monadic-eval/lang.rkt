#lang racket
(require "syntax.rkt"
         "parser.rkt"
         "units.rkt"
         "fix.rkt")
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
    [(_ (l ...) e0 e ...)
     (begin
      (define linkage (syntax->datum #'(link l ...)))
      (define ev-exp (syntax->datum #'e0))
       #`(#%module-begin
          (define-values/invoke-unit/infer #,linkage)
          (define (eval e1) (mrun (#,ev-exp e1)))
          (set-box! run (λ (x) (mret (eval (parse x)))))
          ((unbox run) 'e) ...))]))
