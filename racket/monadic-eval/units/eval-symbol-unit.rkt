#lang racket/unit
(require racket/match
         racket/set
         "../syntax.rkt"
	 "../signatures.rkt")

(import ev^)
(export eval^ unit^ bind^ rec^ err^
        symbolic^ unit-ans^ unit-vals^)

(define (eval e) ((rec e (hash)) (hash)))

(define ((unit v) s) (set (cons v s)))
(define ((err) s) (set (cons (set 'err s))))

(define ((both c0 c1) s)
  (set-union (c0 s) (c1 s)))

(define (rec e r)
  (match e
    [(ifz e0 e1 e2)
     (do v ← (rec e0 r)
       (match v
         [0           (ev e1 r)]
         [(? number?) (ev e2 r)]
         [(? symbolic?)
          (both (ev e1 r)
                (ev e2 r))]))]
    [_  (ev e r)]))


(define ((bind a f) s)
  (for*/fold ([rs (set)])
    ([ans (a s)])
    (set-union rs
      (match ans
        [(cons 'err s)
         (set (cons 'err s))]
        [(cons v s)
         ((f v) s)]))))
  

(define (unit-ans v s) (set (cons v s)))
(define ((unit-vals vs) s)
  (for/set ([v (in-set vs)])
    (cons v s)))


(define symbolic? symbol?)

(define (symbolic-apply f x) (err))

(define-syntax do
  (syntax-rules (←)
    [(do b) b]
    [(do x ← e . r)
     (bind e (λ (x) (do . r)))]))