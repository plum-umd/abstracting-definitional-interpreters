#lang racket/unit
(require racket/match
         racket/set
         "../map.rkt"
         "../signatures.rkt"
         "../unparse.rkt"
         "../transformers.rkt"
         "CacheT.rkt")
(import)
(export monad^ menv^ mstore^ gc^ mcache^)

(define M
  (ReaderT                  ; ρ
  (ReaderT                  ; α
    (FailT
  (NondetT
   (StateT (FinMapO PowerO) ; σ
   (CacheT (FinMapO PowerO) ID)))))))

(define-monad M)

(define (mrun m [ρ₀ ∅] [α₀ (set)] [σ₀ ∅] [Σ⊥₀ ∅] [Σ₀ ∅])
  (run-CacheT  Σ₀ Σ⊥₀
  (run-StateT  σ₀
  (run-ReaderT α₀
  (run-ReaderT ρ₀ m)))))

(define (mret x) x)

;; menv^ impl:

(define ask-env (bind ask (compose1 return car)))

(define (local-env ρ m)
  (do (cons _ α.Σ⊥) ← ask
    (local (cons ρ α.Σ⊥) m)))

;; mstore^ impl:

(define get-store (bind get (compose1 return car)))

(define (put-store σ)
  (do (cons _ Σ) ← get
    (put (cons σ Σ))))

(define (update-store f)
  (do σ ← get-store
    (put-store (f σ))))

;; gc^ impl:

(define ask-roots (bind ask (compose1 return cadr)))

(define (extra-roots rs m)
  (do `(,ρ . (,ψ . ,Σ⊥)) ← ask
    (local `(,ρ . (,(set-union ψ rs) . ,Σ⊥)) m)))

;; mcache^ impl:

(define ask-⊥ (bind ask (compose1 return cddr)))
(define (local-⊥ Σ⊥ m)
  (do `(,ρ . (,α . ,_)) ← ask
    (local `(,ρ . (,α . ,Σ⊥)) m)))

(define get-$ (bind get (compose1 return cdr)))
(define (put-$ Σ)
  (do (cons σ _) ← get
    (put (cons σ Σ))))
(define (update-$ f)
  (do Σ ← get-$
    (put-$ (f Σ))))
