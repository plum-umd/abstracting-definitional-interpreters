#lang racket
(require "../signatures.rkt"
	 "../syntax.rkt")
(provide ev@)

(define-unit ev@
  (import unit^ bind^ rec^ δ^ env^)
  (export ev^)

  (define (ev e ρ)
    (match e
      [(vbl x) (get ρ x)]
      [(num n) (unit n)]
      [(lam x e) (unit (cons (lam x e) ρ))]
      [(ifz e0 e1 e2)
       (do v ← (rec e0 ρ)
	 (match v
	   [0 (rec e1 ρ)]
	   [n (rec e2 ρ)]))]
      [(op1 o e0)
       (do v ← (rec e0 ρ)
	 (δ o v))]
      [(op2 o e0 e1)
       (do v0 ← (rec e0 ρ)
           v1 ← (rec e1 ρ)
	 (δ o v0 v1))]
      [(lrc f (lam x e0) e)
       (do a ← (ralloc f (cons (lam x e0) ρ))
         (rec e (extend-env ρ f a)))]
      [(app e0 e1)
       (do v0 ← (rec e0 ρ)
           v1 ← (rec e1 ρ)
         (match v0
           [(cons (lam x e) ρ0)
	    (do a ← (alloc v0 v1)
	      (rec e (extend-env ρ0 x a)))]))]))
  
   (define-syntax do
    (syntax-rules (←)
      [(do b) b]
      [(do x ← e . r)
       (bind e (λ (x) (do . r)))])))


(define (extend-env r x a)
  (hash-set r x a))


