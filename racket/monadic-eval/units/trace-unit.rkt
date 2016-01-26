#lang racket/unit
(require racket/match
         racket/list
	 "../signatures.rkt")

(import ev^)
(export eval^)

(define (eval e)
  (define (((ev′ e ρ) σ) τ)
    ((((ev e ρ) ev′) σ)
     (cons (list e ρ σ) τ)))
    
  (((ev′ e (hash)) (hash)) empty))