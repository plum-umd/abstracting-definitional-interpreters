#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
         "../map.rkt")
(import)
(export monad^ menv^ mstore^)

;; monad^ impl:

;; M := ρ → σ → ℘((a ∪ (failure)) × σ)
(define M (ReaderT (FailT (StateT #f (NondetT ID)))))
(define-monad M)

;; mrun : (M a) [→ ρ [→ σ]] → ℘((a ∪ (failure)) × σ)
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

;; env^ impl:
(define ask-env   ask)
(define local-env local)

;; store^ impl:
(define get-store get)
(define put-store put)
(define (update-store f)
  (do σ ← get-store
    (put-store (f σ))))
