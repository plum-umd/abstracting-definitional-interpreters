#lang racket
(require racket/match
         (for-syntax racket/syntax syntax/parse)
         "../signatures.rkt"
         "../transformers.rkt"
         "delta.rkt"
         "delta-abs.rkt"
         "delta-pres.rkt")
(provide force-δ@
         force-δ-abs@
         force-δ-pres@)

(define-syntax (define-force-δ@ stx)
  (syntax-parse stx
    [(define-force-δ@ δ@)
     (with-syntax ([force-δ@ (format-id #'δ@ "force-~a"
                                        (syntax->datum #'δ@))])
       #'(define-unit force-δ@
           (import monad^ force^)
           (export δ^)
           (init-depend monad^)

           (define-monad M)

           (define-values/invoke-unit δ@
             (import monad^)
             (export (prefix orig- δ^)))

           (define (δ . ovs)
             (match ovs
               [(list op (and v (list 'var _)))
                (do v′ ← (force v) (δ op v′))]
               [(list op (and v (list 'var _)) u)
                (do v′ ← (force v) (δ op v′ u))]
               [(list op v (and u (list 'var _)))
                (do u′ ← (force u) (δ op v u′))]
               [ovs (apply orig-δ ovs)]))

           (define (truish? v)
             (bind (force v) orig-truish?))))]))

(define-force-δ@ δ-abs@)  ;; force-δ-abs@
(define-force-δ@ δ-pres@) ;; force-δ-pres@
(define-force-δ@ δ@)      ;; force-δ@
