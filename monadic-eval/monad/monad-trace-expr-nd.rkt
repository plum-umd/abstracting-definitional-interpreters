#lang racket/unit
(require racket/set
         "../signatures.rkt"
         "../transformers.rkt"
         "../unparse.rkt"
         "../map.rkt")
(import)
(export monad^ menv^ mstore^)

;; monad^ impl:

;; M := ρ → σ → ℘((a ∪ (failure)) × σ)
(define M (ReaderT (FailT (StateT #f (WriterT ListO (NondetT ID))))))
(define-monad M)

;; mrun : (M a) [→ ρ [→ σ]] → ℘((a ∪ (failure)) × σ)
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ m)))

(define (mret x) 
  (for/set ([y x])
    (unparse-⟨⟨maybe-v⟩×σ⟩×e-seq y)))

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
