#lang racket
(require "syntax.rkt"
	 "map.rkt")
(provide (all-defined-out))

(define (unparse e)
  (match e
    ['err 'err]
    [(num n) n]
    [(vbl s) s]
    [(lam x e) 
     (list 'λ (list x) (unparse e))]
    [(app e0 e1)
     (list (unparse e0) (unparse e1))]
    [(ifz e0 e1 e2)
     (list 'if0 (unparse e0) (unparse e1) (unparse e2))]
    [(op1 o e)
     (list o (unparse e))]
    [(op2 o e0 e1)
     (list o (unparse e0) (unparse e1))]
    [(lrc f e0 e1)
     (list 'rec f (unparse e0) (unparse e1))]
    [(sym s)
     (list 'quote s)]
    [(drf e) 
     (list '! (unparse e))]
    [(srf e0 e1)
     (list (unparse e0) ':= (unparse e1))]
    [(ref e)
     (list 'ref (unparse e))]))

(define (unparse-v v)
  (match v
    [(cons e r) (cons (unparse e) r)]
    [v v]))

(define (unparse-σ σ)
  (hash->map
    (for/hash ([(k v) (∈ σ)])
      (values k (unparse-v v)))))

(define (unparse-⟨maybe-v⟩×σ ans)
  (match ans
    [(cons 'failure σ) (cons 'failure (unparse-σ σ))]
    [(cons v σ) (cons (unparse-v v) (unparse-σ σ))]))

(define (unparse-⟨maybe-v×σ⟩ ans)
  (match ans
    ['failure 'failure]
    [(cons v σ) (cons (unparse-v v) (unparse-σ σ))]))

(define (unparse-⟨⟨maybe-v⟩×σ⟩×⟨e-ρ-σ⟩list x)
  (match x
    [(cons l r) (cons (unparse-⟨maybe-v⟩×σ l)
                      (map unparse-⟨e-ρ-σ⟩ r))]))

(define (unparse-⟨⟨maybe-v⟩×σ⟩×⟨e-ρ-σ⟩set x)
  (match x
    [(cons l r) (cons (unparse-⟨maybe-v⟩×σ l)
                      (map unparse-⟨e-ρ-σ⟩ (set->list r)))]))

(define (unparse-⟨e-ρ-σ⟩ ers)
  (match ers
    [(list e r s) (list (unparse e) r (unparse-σ s))]))