#lang racket/unit
(require "../signatures.rkt")

(import (rename ev^ [ev0 ev]))
(export ev^)

(define ((((ev e ρ) ev) σ) τ)
  ((((ev0 e ρ) ev) σ)
   (cons (list e ρ σ) τ)))

#;
(define (eval e)
  (define (((ev′ e ρ) σ) τ)
    ((((ev0 e ρ) ev′) σ)
     (cons (list e ρ σ) τ)))
    
  (((ev′ e (hash)) (hash)) empty))