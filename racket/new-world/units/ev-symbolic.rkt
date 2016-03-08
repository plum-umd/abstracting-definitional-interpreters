#lang racket/unit

(require racket/match
         "../signatures.rkt" "../syntax.rkt" "../../monad-transformers.rkt")

(import monad^ state^ δ^ env^)
(export ev^)

(define ((ev ev) e)
  (with-monad M
    (match e
      [(vbl x)
       (find x)]
      [(num n)
       (return n)]
      [(sym x)
       (return x)]
      [(ifz e0 e1 e2)
       (do v ← (ev e0)
         n ← (δ 'flip v)
         (case n
           [(0) (ev e2)]
           [(1) (ev e1)]))]
      [(op1 o e0)
       (do v ← (ev e0)
         (δ o v))]
      [(op2 o e0 e1)
       (do v0 ← (ev e0)
         v1 ← (ev e1)
         (δ o v0 v1))]
      [(lrc f (lam x e0) e1)
       (rext f (lam x e0) (ev e1))]
      [(lam x e0)
       (do ρ ← ask-env
         (return (cons (lam x e0) ρ)))]
      [(app e0 e1)
       (do (cons (lam x e) ρ) ← (ev e0)
           v1                 ← (ev e1)
         (local-env ρ (ext x v1 (ev e))))])))
