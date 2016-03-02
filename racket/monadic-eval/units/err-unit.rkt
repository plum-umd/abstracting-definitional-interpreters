#lang racket/unit
(require "../signatures.rkt")
(import monad^)
(export err^)
(define (err) (return 'err))
