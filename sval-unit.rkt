#lang racket/unit
(require racket/match
         "signatures.rkt"
	 "both.rkt")

(import ev^)
(export eval^ unit^ bind^ rec^ fail^ symbolic-monad^)

(define (symbolic? x) (or (symbol? x) (pair? x)))

(define ((both c0 c1) s)
  (both-ans (c0 s) (c1 s)))

(define (symbolic-apply f v)
  (unit `(,f ,v)))

(define (eval e) ((ev e (hash)) (hash)))
(define (rec e r) (ev e r))
(define ((unit v) s) (cons v s))
(define ((fail) s) (cons 'fail s))
(define ((bind a f) s)
  (let loop ([res (a s)])
    (match res
      [(both-ans a1 a2)
       (both-ans (loop a1) (loop a2))]
      [(cons 'fail s) (cons 'fail s)]
      [(cons v s)
       ((f v) s)])))
