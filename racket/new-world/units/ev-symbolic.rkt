#lang racket/unit

(require racket/match
         "../signatures.rkt" "../syntax.rkt" "../../monad-transformers.rkt")

(import monad^ state^ δ^ env^ ref^)
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
      [(ref e)
       (do v ← (ev e)
           (mkbox v))]
      [(drf e)
       (do v ← (ev e)
           (ubox v))]
      [(srf e0 e1)
       (do a  ← (ev e0)
           v  ← (ev e1)
           (sbox a v))]
      [(lrc f (lam x e0) e1)
       (rext f (lam x e0) (ev e1))]
      [(lam x e0)
       (do ρ ← ask-env
           (return (cons (lam x e0) ρ)))]
      [(app e0 e1)
       (do v0 ← (ev e0)
           v1 ← (ev e1)
           (match v0
             [(cons (lam x e) ρ)
              (local-env ρ (ext x v1 (ev e)))]
             [_ fail]))])))