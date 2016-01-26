#lang racket/unit
(require racket/match
         "../signatures.rkt"
         "../store.rkt"
	 "../both.rkt")

(import)
(export unit^ bind^ symbolic^ ref^)

(define (symbolic? x) (or (symbol? x) (pair? x)))

(define ((both c0 c1) s)
  (both-ans (c0 s) (c1 s)))

(define (symbolic-apply f v)
  (unit `(,f ,v)))

(define ((unit v) s) (cons v s))
(define ((bind a f) s)
  (let loop ([res (a s)])
    (match res
      [(both-ans a1 a2)
       (both-ans (loop a1) (loop a2))]
      [(cons 'err s) (cons 'err s)]
      [(cons v s)
       ((f v) s)])))

(define ((new v) s)
  (define a (next s))
  ((unit a) (update-sto s a v)))

(define ((sbox a v) s)
  ((unit a) (update-sto s a v)))

(define ((ubox a) s)
  ((unit (lookup-sto s a)) s))