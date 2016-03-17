#lang racket/unit
(require racket/set
         "../map.rkt"
         "../signatures.rkt"
         "../transformers.rkt"
         "../unparse.rkt"
         "CacheT.rkt")
(import)
(export monad^ mlive^ mdead^ menv^ mstore^ mcache^)

;; M ρ θ σ Σ⊥ Σ a := ρ → θ → σ → Σ⊥ → Σ → (℘((a ∪ (failure)) × θ) × σ) × Σ
(define M
  (ReaderT                  ; ρ
  (ReaderT                  ; α
    (FailT
   (StateT #f               ; θ
  (NondetT
   (StateT (FinMapO PowerO) ; σ
   (CacheT (FinMapO PowerO) ID))))))))

(define-monad M)

(define (mrun m [ρ₀ ∅] [α₀ ∅] [θ₀ {set}] [σ₀ ∅] [Σ⊥₀ ∅] [Σ₀ ∅])
  (run-CacheT  Σ₀ Σ⊥₀
  (run-StateT  σ₀
  (run-StateT  θ₀
  (run-ReaderT α₀
  (run-ReaderT ρ₀ m))))))

;; placeholder
(define (mret x)
  (let ([wo-cache (ret-CacheT x)])
    (cons (car wo-cache) (cdr wo-cache))))

;; menv^ impl:

(define ask-env (bind ask (compose1 return car)))

(define (local-env ρ m)
  (do (cons _ α.Σ⊥) ← ask
    (local (cons ρ α.Σ⊥) m)))

;; mstore^ impl:

(define get-store (with-monad M (bind get (compose1 return cadr))))
(define (put-store σ)
  (with-monad M
    (do `(,θ . (,_ . ,Σ)) ← get
        (put `(,θ . (,σ . ,Σ))))))

(define (update-store f)
  (do σ ← get-store
      (put-store (f σ))))

;; dead^ impl:

(define get-dead (bind get (compose1 return car)))

(define (put-dead θ)
  (do (cons _ σ×Σ) ← get
      (put (cons θ σ×Σ))))
(define (update-dead f)
  (with-monad M
    (do
      θ ← get-dead
      (put-dead (f θ)))))

;; mlive^ impl:

(define ask-live (bind ask (compose1 return cadr)))

(define (local-live α m)
  (do `(,ρ . (,_ . ,Σ⊥)) ← ask
    (local `(,ρ . (,α . ,Σ⊥)) m)))

;; mcache^ impl:

(define ask-⊥ (bind ask (compose1 return cddr)))
(define (local-⊥ Σ⊥ m)
  (do `(,ρ . (,α . ,_)) ← ask
    (local `(,ρ . (,α . ,Σ⊥)) m)))

(define get-$ (bind get (compose1 return cddr)))
(define (put-$ Σ)
  (do `(,σ . (,θ . ,_)) ← get
    (put `(,σ . (,θ . ,Σ)))))
(define (update-$ f)
  (do Σ ← get-$
    (put-$ (f Σ))))

