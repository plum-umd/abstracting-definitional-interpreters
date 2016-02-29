#lang racket/unit
(require "../signatures.rkt")

(import)
(export fix^)
(define (fix f)
  (λ (e ρ) ((f e ρ) (fix f))))