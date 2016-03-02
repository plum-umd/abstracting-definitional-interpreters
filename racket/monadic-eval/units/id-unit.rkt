#lang racket/unit
(require "../signatures.rkt" racket/match)
(import)
(export return^ bind^ run^)

(define (mrun M) M)
(define (return v) v)
(define (bind a f)
  (match a
    ['err 'err]
    [v    (f v)]))
