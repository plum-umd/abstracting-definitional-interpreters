#lang racket
(require "../signatures.rkt"
	 "../syntax.rkt")
(provide ev@)

(define-unit ev@
  (import monad^ δ^ env^ sto^)
  (export ev^)
  
  (define ((ev e ρ) ev)
    (match e
      [(vbl x) (get ρ x)]
      [(num n)
       (return n)]
      [(lam x e) (return (cons (lam x e) ρ))]
      [(ifz e0 e1 e2)
       (do v ← (ev e0 ρ)
         (do b ← (truish? v)
           (if b
               (ev e1 ρ)
               (ev e2 ρ))))]
      [(op1 o e0)
       (do v ← (ev e0 ρ)
         (δ o v))]
      [(op2 o e0 e1)
       (do v0 ← (ev e0 ρ)
           v1 ← (ev e1 ρ)
         (δ o v0 v1))]
      [(lrc f (lam x e0) e)
       (do a ← (ralloc f (lam x e0) ρ)
         (ev e (ext ρ f a)))]
      [(app e0 e1)
       (do v0 ← (ev e0 ρ)
           v1 ← (ev e1 ρ)
         (match v0
           [(cons (lam x e) ρ0)
            (do a ← (alloc v0 v1)
              (ev e (ext ρ0 x a)))]))]))
  
  (define-syntax do
    (syntax-rules (←)
      [(do b) b]
      [(do x ← e . r)
       (bind e (λ (x) (do . r)))])))


