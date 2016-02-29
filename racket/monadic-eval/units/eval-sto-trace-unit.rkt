#lang racket/unit
(require "../signatures.rkt")

(import ev^ fix^)
(export eval^)

(define (eval e)
  ((((fix ev) e (hash)) (hash)) '()))