#lang racket/unit
(require racket/match
	 "signatures.rkt")

(import ev^)
(export eval^ unit^ bind^)

(define (eval e) ((ev e (hash)) (hash)))
(define (rec e r) (ev e r))
(define ((unit v) s) (cons v s))
(define ((bind a f) s)
  (match (a s)
    [(cons v s) ((f v) s)]))
