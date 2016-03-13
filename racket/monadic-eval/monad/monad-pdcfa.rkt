#lang racket/unit
(require "../map.rkt"
         "../signatures.rkt"
         "../transformers.rkt")
(import)
(export monad^ menv^ mstore^ mcache^)

;; M ρ σ Σ⊥ Σ a := ρ → σ → Σ⊥ → Σ → ℘(((a ∪ (failure)) × σ)) × Σ
(define M
  (ReaderT                  ; ρ
    (FailT
   (StateT #f               ; σ
  (NondetT
  (ReaderT                  ; Σ⊥
   (StateT (FinMapO PowerO) ; Σ
        ID)))))))

(define-monad M)

(define (mrun m [ρ₀ ∅] [σ₀ ∅] [Σ⊥₀ ∅] [Σ₀ ∅])
  (run-StateT  Σ₀
  (run-ReaderT Σ⊥₀
  (run-StateT  σ₀
  (run-ReaderT ρ₀ m)))))

;; menv^ impl:

(define ask-env (bind ask (compose1 return car)))

(define (local-env ρ m)
  (do (cons _ Σ⊥) ← ask
    (local (cons ρ Σ⊥) m)))


;; mstore^ impl:

(define get-store (bind get (compose1 return car)))

(define (put-store σ)
  (do (cons _ Σ) ← get
    (put (cons σ Σ))))

(define (update-store f)
  (do σ ← get-store
    (put-store (f σ))))


;; mcache^ impl:

(define ask-⊥ (bind ask (compose1 return cdr)))
(define (local-⊥ Σ⊥ m)
  (do (cons ρ _) ← ask
    (local (cons ρ Σ⊥) m)))

(define get-$ (bind get (compose1 return cdr)))
(define (put-$ Σ)
  (do (cons σ _) ← get
    (put (cons σ Σ))))
(define (update-$ f)
  (do Σ ← get-$
    (put-$ (f Σ))))
