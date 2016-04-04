#lang racket/unit

(require "../signatures.rkt"
         "../transformers.rkt")

(import monad^)
(export alloc^)

(define-monad M)

(define (alloc x) (return x))