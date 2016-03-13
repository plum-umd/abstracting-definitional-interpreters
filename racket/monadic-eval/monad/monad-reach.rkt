#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
	 "../map.rkt")
(import)
(export monad^ menv^ mstore^ mreach^)

;; M ρ σ a := ρ → σ → (a × ℘(a)) × σ
(define M (ReaderT (StateT #f (WriterT PowerO (FailT ID)))))

;; mrun : (M ρ θ σ a) [→ ρ [→ θ [→ σ]]] → (a × ℘(a)) × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

(define-monad M)

;; env^ impl:
(define ask-env   ask)
(define local-env local)

;; store^ impl:
(define get-store get)
(define put-store put)
(define (update-store f)
  (do σ ← get-store
      (put-store (f σ))))

;; reach^ impl:
(define tell-reach   tell)
(define hijack-reach hijack)
