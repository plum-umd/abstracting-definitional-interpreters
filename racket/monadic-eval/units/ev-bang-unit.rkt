#lang racket
(require "../signatures.rkt"
	 "../syntax.rkt")
(provide ev!@)

(define-unit ev!@
  (import return^ bind^ δ^ env^ sto^ err^ ref^)
  (export ev^)

  (define-syntax do
    (syntax-rules (←)
      [(do b) b]
      [(do x ← e . r)
       (bind e (λ (x) (do . r)))]))

  (define ((ev e ρ) ev)
    (match e
      ['err (err)]
      [(vbl x) (get ρ x)]
      [(num n) (return n)]
      [(lam x e) (return (cons (lam x e) ρ))]
      [(ifz e0 e1 e2)
       (do v ← (ev e0 ρ)
	 (match v
	   [0 (ev e1 ρ)]
	   [n (ev e2 ρ)]))]
      [(op1 o e0)
       (do v ← (ev e0 ρ)
	 (δ o v))]
      [(op2 o e0 e1)
       (do v0 ← (ev e0 ρ)
           v1 ← (ev e1 ρ)
	 (δ o v0 v1))]
      [(ref e)
       (do v ← (ev e ρ)
         (new v))]
      [(drf e)
       (do a ← (ev e ρ)
	 (ubox a))]
      [(srf e0 e1)
       (do a ← (ev e0 ρ)
           v ← (ev e1 ρ)
         (sbox a v))]
      [(lrc f (lam x e0) e)
       (do a ← (ralloc f (lam x e0) ρ)
         (ev e (ext ρ f a)))]
      [(app e0 e1)
       (do v0 ← (ev e0 ρ)
           v1 ← (ev e1 ρ)
         (match v0
           [(cons (lam x e) r0)
	    (do a ← (alloc (cons (lam x e) r0) v1)
	      (ev e (ext r0 x a)))]))])))

