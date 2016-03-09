#lang racket
(provide fix)
(define (fix f)
  (λ (e ρ) ((f e ρ) (fix f))))