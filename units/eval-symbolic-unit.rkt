#lang racket/unit
(require racket/match
         "../syntax.rkt"
	 "../signatures.rkt")

(import ev^)
(export eval^ unit^ bind^ rec^ err^)

(struct branch (vc t f) #:transparent)

(define (eval e) ((rec e (hash)) (hash)))

(define ((rec e r) s)
  (match e
    [(ifz e0 e1 e2)
     (branch (list e0 r s)
             ((rec e1 r) s)
             ((rec e2 r) s))]
    [_  ((ev e r) s)]))

(define ((unit v) s) (cons v s))
(define ((err) s) (cons 'err s))
(define ((bind a f) s)
  (let loop ([res (a s)])
    (match res
      [(branch vc a1 a2)
       (branch vc (loop a1) (loop a2))]
      [(cons 'err s) (cons 'err s)]
      [(cons v s)
       ((f v) s)])))
