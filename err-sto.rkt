#lang racket/unit
(require "signatures.rkt")
(import)
(export fail^)
(define ((fail) s) (cons 'fail s))
