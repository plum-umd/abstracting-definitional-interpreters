#lang racket
(require "unit-sig.rkt"
         "bind-sig.rkt"
	 "delta-sig.rkt"
	 "env-sig.rkt"
         "ev-sig.rkt"
         "syntax.rkt")

(provide ev@)

(define-unit ev@
  (import unit^ bind^ δ^ env^)
  (export ev^)

  (define-syntax do
    (syntax-rules (←)
      [(do b) b]
      [(do x ← e . r)
       (bind e (λ (x) (do . r)))]))

  (define (ev e r) ;; E R -> [M Ans]
    (match e
      [(vbl x) (get r x)]
      [(num n) (unit n)]
      [(lam x e) (unit (cons (lam x e) r))]
      [(ifz e0 e1 e2)
       (do v ← (rec e0 r)
	 (match v
	   [0 (rec e1 r)]
	   [n (rec e2 r)]))]
      [(op1 o e0)
       (do v ← (rec e0 r)
	 (δ o v))]
      [(op2 o e0 e1)
       (do v0 ← (rec e0 r)
           v1 ← (rec e1 r)
	 (δ o v0 v1))]
      [(lrc f (lam x e0) e)
       (do a ← (ralloc f (cons (lam x e0) r))
         (rec e (extend-env r f a)))]
      [(app e0 e1)
       (do v0 ← (rec e0 r)
           v1 ← (rec e1 r)
         (match v0
           [(cons (lam x e) r0)
	    (do a ← (alloc (cons (lam x e) r0) v1)
	      (rec e (extend-env r0 x a)))]))])))


(define (extend-env r x a)
  (hash-set r x a))
