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

(define-syntax (define-lazy-gc-state@ stx)
  (syntax-parse stx
    [(define-lazy-gc-state@ state@)
     (with-syntax ([lazy-gc-state@ (format-id #'state@ "lazy-gc-~a"
                                              (syntax->datum #'state@))])
       #'(define-unit lazy-gc-state@
           (import monad^ gc-count^ menv^ mstore^ mlive^)
           (export state^ force^)
           (init-depend monad^)

           (define-values/invoke-unit state@
             (import monad^ menv^ mstore^)
             (export (prefix orig- state^)))

           (define-monad M)

           (define (find a)
             (do (update-live (λ (α) (α a (add1 (if (∈ a α) (α a) 0)))))
                 (return (list 'var a))))

           (define (force v)
             (match v
               [(list 'var a)
                (do v′ ← (orig-find a)
                    α  ← get-live
                    n  ≔ (sub1 (α a))
                    (if (eqv? n 0)
                        (do (update-store (λ (σ) (map-rem σ a)))
                            (update-live  (λ (α) (map-rem α a))))
                        (update-live (λ (α) (α a n))))
                    (return v′))]
               [_ (return v)]))

           (define (ext a v)
             (do v′ ← (force v)
                 (orig-ext a v′)))))]))

(define-lazy-state@ state@)
(define-lazy-state@ state-crush@)
(define-lazy-state@ state-nd@)

(define-lazy-gc-state@ state@)
(define-lazy-gc-state@ state-crush@)
(define-lazy-gc-state@ state-nd@)
