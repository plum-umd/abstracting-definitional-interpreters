#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
	 "../map.rkt")
(import)
(export monad^ menv^ mstore^)

;; monad^ impl:

;; M := ρ → σ → (a × σ)
(define M (ReaderT (FailT (StateT #f ID))))

;; mrun : (M a) [→ ρ [→ θ [→ σ]]] → a × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env    (with-monad M local))

;; store^ impl:
(define get-store (with-monad M get))
(define put-store (with-monad M put))
(define (update-store f)
  (with-monad M
    (do
      σ ← get-store
      (put-store (f σ)))))
