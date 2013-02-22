#lang racket
(require "../signatures.rkt")
(provide eval!@)

(define-unit eval!@
  (import ev^)
  (export eval^ unit^ bind^ rec^ err^)
  (define (eval e) (ev e (hash)))
  (define (rec e r) (ev e r))
  (define (unit v) v)
  (define (err) 'err)
  (define (bind a f)
    (match a
      ['err 'err]
      [v    (f v)])))
