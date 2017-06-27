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
    [(lrc f e)
     (list 'rec f (unparse e))]
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
    (for/hash ([(k v) (∈ σ)] #:unless (equal? k '_)) ; HACK!
      (values k 
              (if (set? v)
                  (for/list ([v v]) (unparse-v v))
                  (unparse-v v))))))

(define (unparse-⟨⟨maybe-v⟩×σ⟩set/discard-σ ans)
  (for/list ([x ans])
    (unparse-⟨maybe-v⟩ (car x))))
  
(define (unparse-⟨⟨maybe-v⟩×σ⟩set ans)
  (for/list ([x ans])
    (unparse-⟨maybe-v⟩×σ x)))

(define (unparse-⟨maybe-v⟩ x)
  (match x
    ['failure 'failure]
    [v (unparse-v v)]))

(define (unparse-⟨maybe-v⟩×σ ans)
  (match ans
    [(cons 'failure σ) (cons 'failure (unparse-σ σ))]
    [(cons v σ) (cons (unparse-v v) (unparse-σ σ))]))

(define (unparse-⟨maybe-v×σ⟩ ans)
  (match ans
    ['failure 'failure]
    [(cons v σ) (cons (unparse-v v) (unparse-σ σ))]))

(define (unparse-⟨⟨maybe-v⟩×σ⟩×⟨e⟩set x)
  (match x
    [(cons as es)
     (cons (unparse-⟨maybe-v⟩×σ as)
           (for/set ([e es]) (unparse e)))]))

(define (unparse-⟨⟨maybe-v⟩×σ⟩×⟨e-ρ-σ⟩seq x)
  (match x
    [(cons l r) (cons (unparse-⟨maybe-v⟩×σ l)
                      (map unparse-⟨e-ρ-σ⟩ (for/list ([x r]) x)))]))

(define (unparse-⟨e-ρ-σ⟩ ers)
  (match ers
    [(list e r s) (list (unparse e) r (unparse-σ s))]))

(define (unparse-⟨maybe-v⟩set×σ x)
  (match x
    [(cons vs s) (cons (unparse-⟨maybe-v⟩set vs) (unparse-σ s))]))

(define (unparse-⟨maybe-v⟩set vs)
  (for/list ([v vs])
    (unparse-⟨maybe-v⟩ v)))
