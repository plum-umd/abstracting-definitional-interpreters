#lang racket/unit
(require racket/match
	 "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt")

(import)
(export ref^)

(define ((new v) s)  
  (define a (next s))
  (cons a (update-sto s a v)))

(define ((sbox a v) s)
  (cons a (update-sto s a v)))

(define ((ubox a) s)
  (cons (lookup-sto s a) s))