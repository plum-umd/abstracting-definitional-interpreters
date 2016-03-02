#lang racket/unit
(require racket/match
         "../signatures.rkt"
         "../store.rkt"
	 "../both.rkt")

(import)
(export return^ bind^ symbolic^ ref^ run^)

(define (mrun M) (M (hash)))

(define (symbolic? x) (or (symbol? x) (pair? x)))

(define ((both c0 c1) s)
  (both-ans (c0 s) (c1 s)))

(define (symbolic-apply f v)
  (return `(,f ,v)))

(define ((return v) s) (cons v s))
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
  ((return a) (update-sto s a v)))

(define ((sbox a v) s)
  ((return a) (update-sto s a v)))

(define ((ubox a) s)
  ((return (lookup-sto s a)) s))