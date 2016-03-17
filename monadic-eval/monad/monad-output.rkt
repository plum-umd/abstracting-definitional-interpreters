#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
         "../unparse.rkt"
         "../map.rkt")
(import monoid^)
(export monad^ menv^ mstore^)

;; M O ρ σ a := ρ → σ → (a × O a) × σ
(define M (ReaderT (FailT (StateT #f (WriterT O ID)))))
(define-monad M)

;; mrun : (M O ρ σ a) [→ ρ [→ σ]] → (a × O a) × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

(define mret unparse-⟨⟨maybe-v⟩×σ⟩×⟨e-ρ-σ⟩seq)

;; env^ impl:
(define ask-env   ask)
(define local-env local)

;; store^ impl:
(define get-store get)
(define put-store put)
(define (update-store f)
  (do σ ← get-store
      (put-store (f σ))))
