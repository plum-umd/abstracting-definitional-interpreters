#lang racket/unit
(require "../signatures.rkt"
         "../../monad-transformers.rkt")
(import)
(export monad^ menv^ mstore^)

;; monad^ impl:

;; M := ρ → σ → (a × σ)
(define M (ReaderT (StateT #f (FailT ID))))

;; mrun : (M a) [→ ρ [→ θ [→ σ]]] → a × σ
(define (mrun m [ρ₀ (hash)] [σ₀ (hash)])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env    (with-monad M local))

;; store^ impl:
(define get-store (with-monad M get))
(define put-store (with-monad M put))
