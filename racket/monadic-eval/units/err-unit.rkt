#lang racket/unit
(require "../signatures.rkt")
(import return^)
(export err^)
(define (err) (return 'err))
