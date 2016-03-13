#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
	 "../map.rkt")
(import)
(export monad^ menv^ mstore^ state^)

;; monad^ impl:

;; M := ρ → σ → (a × σ)
(define M (ReaderT (StateT #f (FailT ID))))
(define-monad M)

;; mrun : (M a) [→ ρ [→ θ [→ σ]]] → a × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

;; env^ impl:
(define ask-env ask)
(define local-env local)

;; store^ impl:
(define get-store get)
(define put-store put)
(define (update-store f)
  (do σ ← get-store
      (put-store (f σ))))


;; In-lined from metafunctions

(define (find a)
  (do σ ← get-store
      (return (σ a))))

(define (ext a v) (update-store (λ (σ) (σ a v))))
