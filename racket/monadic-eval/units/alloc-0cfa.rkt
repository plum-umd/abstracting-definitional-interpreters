#lang racket/unit

(require "../signatures.rkt"
         "../transformers.rkt")

(import monad^)
(export alloc^)

(define alloc (with-monad M return))
