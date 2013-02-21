#lang racket
(require "signatures.rkt")
(provide eval!@)

(define-unit eval!@
  (import ev^)
  (export eval^ unit^ bind^ fail^)
  (define (eval e) (ev e (hash)))
  (define (rec e r) (ev e r))
  (define (unit v) v)
  (define (fail) 'fail)
  (define (bind a f)
    (match a
      ['fail 'fail]
      [v     (f v)])))
