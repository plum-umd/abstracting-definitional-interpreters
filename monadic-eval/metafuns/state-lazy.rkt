#lang racket
(require (for-syntax racket/syntax syntax/parse)
         "../map.rkt"
         "../signatures.rkt"
         "../transformers.rkt"
         "state.rkt"
         "state-crush.rkt"
         "state-nd.rkt")
(provide (all-defined-out))

(define-syntax (define-lazy-state@ stx)
  (syntax-parse stx
    [(define-lazy-state@ state@)
     (with-syntax ([lazy-state@ (format-id #'state@ "lazy-~a"
                                           (syntax->datum #'state@))])
       #'(define-unit lazy-state@
           (import monad^ menv^ mstore^)
           (export state^ force^)
           (init-depend monad^)

           (define-values/invoke-unit state@
             (import monad^ menv^ mstore^)
             (export (prefix orig- state^)))

           (define-monad M)

           (define (find a) (return (list 'var a)))

           (define (force v)
             (match v
               [(list 'var a) (orig-find a)]
               [_             (return v)]))

           (define (ext a v)
             (do v′ ← (force v)
                 (orig-ext a v′)))))]))

(define-lazy-state@ state@)
(define-lazy-state@ state-crush@)
(define-lazy-state@ state-nd@)
