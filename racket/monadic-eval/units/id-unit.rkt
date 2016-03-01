#lang racket/unit
(require "../signatures.rkt" racket/match)
(import)
(export unit^ bind^ run^)

(define (mrun M) M)
(define (unit v) v)
(define (bind a f)
  (match a
    ['err 'err]
    [v    (f v)]))
