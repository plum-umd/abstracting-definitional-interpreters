#lang racket/unit

(require racket/match
         "../syntax.rkt"
         "../../monad-transformers.rkt"
         "../signatures.rkt")

(import monad^ alloc^ state^ δ^)
(export ev^)

(define ((ev ev) e)
  (with-monad M
    (match e
      [(vbl x)               (find x)]
      [(num n)               (return n)]
      [(ifz e0 e1 e2)        (do v ← (ev e0)
                               (do b ← (truish? v)
                                 (ev (if b e1 e2))))]
      [(op1 o e0)            (do v ← (ev e0)
                               (δ o v))]
      [(op2 o e0 e1)         (do v0 ← (ev e0)
                                 v1 ← (ev e1)
                               (δ o v0 v1))]
      [(lrc f (lam x e0) e1) (rext f (lam x e0) (ev e1))]
      [(lam x e0)            (do ρ ← ask
                               (return (cons (lam x e0) ρ)))]
      [(app e0 e1)           (do (cons (lam x e) ρ) ← (ev e0)
                                 v1                 ← (ev e1)
                               (local ρ (ext x v1 (ev e))))])))
