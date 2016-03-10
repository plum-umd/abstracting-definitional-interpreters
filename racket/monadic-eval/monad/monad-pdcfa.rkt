#lang racket/unit
(require "../map.rkt"
         "../signatures.rkt"
         "../transformers.rkt")
(import)
(export monad^ menv^ mstore^ mcache^)

;; M ρ σ Σ⊥ Σ a := ρ → σ → Σ⊥ → Σ → ℘(a × σ) × Σ
(define M
  (ReaderT                  ; ρ
  (NondetT
   (StateT (FinMapO PowerO)               ; σ
  (ReaderT                  ; Σ⊥
   (StateT (FinMapO PowerO) ; Σ
        ID))))))

;; mrun : (M ρ σ Σ⊥ Σ a) [→ σ [→ ρ [→ Σ⊥ [→ Σ]]]] → (a × Σ) × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅] [Σ⊥₀ ∅] [Σ₀ ∅])
  (run-StateT  Σ₀
  (run-ReaderT Σ⊥₀
  (run-StateT  σ₀
  (run-ReaderT ρ₀ m)))))

;; menv^ impl:

(define ask-env (with-monad M (bind ask (compose1 return car))))

(define (local-env ρ m)
  (with-monad M
    (do (cons _ Σ⊥) ← ask
      (local (cons ρ Σ⊥) m))))


;; mstore^ impl:

(define get-store (with-monad M (bind get (compose1 return cdr))))

(define (put-store σ)
  (with-monad M
    (do (cons Σ _) ← get
      (put (cons Σ σ)))))

(define (update-store f)
  (with-monad M
    (do σ ← get-store
        (put-store (f σ)))))


;; mcache^ impl:

(define ask-⊥ (with-monad M (bind ask (compose1 return cdr))))
(define (local-⊥ Σ⊥ m)
  (with-monad M
    (do (cons ρ _) ← ask
      (local (cons ρ Σ⊥) m))))

(define get-$ (with-monad M (bind get (compose1 return car))))
(define (put-$ Σ)
  (with-monad M
    (do (cons _ σ) ← get
      (put (cons Σ σ)))))
(define (update-$ f)
  (with-monad M
    (do Σ ← get-$
        (put-$ (f Σ)))))
