#lang racket/unit
(require racket/match
         racket/set
         "../map.rkt"
         "../signatures.rkt"
         "../unparse.rkt"
         "../transformers.rkt"
         "CacheT.rkt")
(import)
(export monad^ menv^ mstore^ mlive^ mcache^)

;; M ρ α σ Σ⊥ Σ a := ρ → α → σ → Σ⊥ → Σ → ℘(a × σ) × Σ
(define M
  (ReaderT                  ; ρ
    (FailT
   (StateT #f               ; α
  (NondetT
   (StateT (FinMapO PowerO) ; σ
   (CacheT (FinMapO PowerO) ID)))))))

(define-monad M)

(define (mrun m [ρ₀ ∅] [α₀ ∅] [σ₀ ∅] [Σ⊥₀ ∅] [Σ₀ ∅])
  (run-CacheT  Σ₀ Σ⊥₀
  (run-StateT  σ₀
  (run-StateT  α₀
  (run-ReaderT ρ₀ m)))))

(define (mret x)
  (for/set ([v×α×σ (ret-CacheT x)])
    (cons (caar v×α×σ) (cdr v×α×σ))))

;; menv^ impl:

(define ask-env (bind ask (compose1 return car)))

(define (local-env ρ m)
  (do (cons _ Σ⊥) ← ask
    (local (cons ρ Σ⊥) m)))

;; mstore^ impl:
(define get-store (bind get (compose1 return cadr)))

(define (put-store σ)
  (do `(,α . (,_ . ,Σ)) ← get
    (put `(,α . (,σ . ,Σ))))) 

(define (update-store f)
  (do σ ← get-store
    (put-store (f σ))))

;; mlive^ impl:

(define get-live (bind get (compose1 return car)))

(define (put-live α)
  (do (cons _ σ×Σ) ← get
    (put (cons α σ×Σ))))


(define (update-live f)
  (do θ ← get-live
    (put-live (f θ))))

;; mcache^ impl:

(define ask-⊥ (bind ask (compose1 return cdr)))
(define (local-⊥ Σ⊥ m)
  (do (cons ρ _) ← ask
    (local (cons ρ Σ⊥) m)))

(define get-$ (bind get (compose1 return cddr)))
(define (put-$ Σ)
  (do `(,α . (,σ . ,_)) ← get
    (put `(,α . (,σ . ,Σ)))))
(define (update-$ f)
  (do Σ ← get-$
    (put-$ (f Σ))))

(define get-state 
  (do α ← get-live
      σ ← get-store
      (return (list α σ))))

(define put-state
  (match-lambda
    [(list α σ) (do (put-live α) (put-store σ))]))
