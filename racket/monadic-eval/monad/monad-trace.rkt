#lang racket/unit
(require "../signatures.rkt"
         "../transformers.rkt"
	 "../map.rkt")
(import)
(export monad^ menv^ mstore^ mtrace^)

;; M ρ σ a := ρ → σ → (a × a list) × σ
(define M (ReaderT (StateT #f (WriterT ListO (FailT ID)))))
(define-monad M)

;; mrun : (M ρ σ a) [→ ρ [→ θ [→ σ]]] → (a × a list) × σ
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

;; trace^ impl:
(define tell-trace   tell)
(define hijack-trace hijack)
