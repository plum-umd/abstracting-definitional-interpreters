#lang racket/unit
(require "../signatures.rkt")
(import unit^)
(export err^)
(define (err) (unit 'err))
