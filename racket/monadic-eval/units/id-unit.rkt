#lang racket/unit
(require "../signatures.rkt" racket/match)
(import)
(export unit^ bind^)

(define (unit v) v)
(define (bind a f)
  (match a
    ['err 'err]
    [v    (f v)]))
