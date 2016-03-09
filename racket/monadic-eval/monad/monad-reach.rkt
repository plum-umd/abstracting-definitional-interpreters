#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt")
(import)
(export monad^ menv^ mstore^ mreach^)

;; M ρ σ a := ρ → σ → (a × ℘(a)) × σ
(define M (ReaderT (StateT #f (WriterT PowerO (FailT ID)))))

;; mrun : (M ρ θ σ a) [→ ρ [→ θ [→ σ]]] → (a × ℘(a)) × σ
(define (mrun m [ρ₀ (hash)] [σ₀ (hash)])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

;; store^ impl:
(define get-store (with-monad M get))
(define put-store (with-monad M put))

;; reach^ impl:
(define tell-reach   (with-monad M tell))
(define hijack-reach (with-monad M hijack))
