#lang racket/unit
(require "../signatures.rkt")

(import rec^)
(export eval^)

(define (eval e) ((rec e (hash)) (hash)))
