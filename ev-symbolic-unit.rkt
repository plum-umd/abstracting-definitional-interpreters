#lang racket/unit
(require "ev-monad-sig.rkt"
         "symbolic-monad-sig.rkt"
         "ev-sig.rkt"         
         "syntax.rkt"
         (only-in racket match))

(import ev-monad^ return^ δ^ symbolic-monad^)
(export ev^)

(define-syntax do
  (syntax-rules (←)
    [(do b) b]
    [(do x ← e . r)
     (bind e (λ (x) (do . r)))]))

(define (ev e r) ;; E R -> [M Ans] 
  (match e
    ['fail (fail)]
    [(vbl x)
     (lookup-env r x)]
    [(sym s) (return s)]
    [(num n) (return n)]
    [(ifz e0 e1 e2)
     (do v ← (rec e0 r)
       (match v
         [0           (rec e1 r)]
         [(? number?) (rec e2 r)]
         [(? symbolic?) 
          (do v0 ← (rec e1 r)
              v1 ← (rec e2 r)
            (both v0 v1))]))]
    [(op1 o e0)
     (do v ← (rec e0 r) 
       (δ o v))]
    [(op2 o e0 e1)
     (do v0 ← (rec e0 r)
         v1 ← (rec e1 r)
       (δ o v0 v1))]
    [(ref e)
     (do v ← (rec e r)
       (new v))]
    [(ubx e)
     (do a ← (rec e r)
       (ubox a))]
    [(sbx e0 e1)
     (do a ← (rec e0 r)
         v ← (rec e1 r)
       (sbox a v))]
    [(lam x e)
     (return (cons (lam x e) r))]
    [(app e0 e1)
     (do v0 ← (rec e0 r)
         v1 ← (rec e1 r)
       (match v0
         [(cons (lam x e) r0)
          (do a ← (alloc (cons (lam x e) r0) v1)
            (rec e (extend-env r0 x a)))]
         [(? symbolic?)
          (symbolic-apply v0 v1)]))]))


(define (extend-env r x a)
  (hash-set r x a))