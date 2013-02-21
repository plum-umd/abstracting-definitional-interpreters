#lang racket/unit
(require "signatures.rkt")
(import)
(export err^)
(define ((err) s) (cons 'err s))
