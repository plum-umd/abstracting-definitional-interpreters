#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
	 "../map.rkt")
(import)
(export monad^ menv^ mstore^ mtrace^)

;; M ρ σ a := ρ → σ → (a × a list) × σ
(define M (ReaderT (StateT #f (WriterT ListO (FailT ID)))))

;; mrun : (M ρ σ a) [→ ρ [→ θ [→ σ]]] → (a × a list) × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

;; store^ impl:
(define get-store (with-monad M get))
(define put-store (with-monad M put))
(define (update-store f)
  (with-monad M
    (do
      σ ← get-store
      (put-store (f σ)))))

;; trace^ impl:
(define tell-trace   (with-monad M tell))
(define hijack-trace (with-monad M hijack))
