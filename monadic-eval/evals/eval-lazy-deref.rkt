#lang racket
(require "../transformers.rkt"
         "../signatures.rkt")
(provide eval-lazy!@)

(define-unit eval-lazy!@
  (import force^ monad^)
  (export eval-lazy!^)

  ;; eval-dead : (e → M v) → e → M v
  (define ((eval-lazy! eval) e₀)
    (with-monad M (bind (eval e₀) force))))
