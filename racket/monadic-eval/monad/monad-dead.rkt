#lang racket/unit
(require racket/set
         "../signatures.rkt"
         "../transformers.rkt")
(import)
(export monad^ menv^ mstore^ mdead^)

;; M ρ σ θ a := ρ → σ → θ → (a × σ) × θ
(define M (ReaderT (StateT #f (StateT #f (FailT ID)))))

;; mrun : (M ρ σ θ a) [→ ρ [→ σ [→ θ]]] → (a × σ) × θ
(define (mrun m [ρ₀ (hash)] [σ₀ (hash)] [θ₀ {set}])
  (run-StateT θ₀ (run-StateT σ₀ (run-ReaderT ρ₀ m))))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

;; store^ impl:
(define get-store (with-monad M (bind get (compose1 return car))))
(define (put-store σ)
  (with-monad M
    (do (cons _ θ) ← get
      (put (cons σ θ)))))

;; dead^ impl:
(define get-dead (with-monad M (bind get (compose1 return cdr))))
(define (put-dead θ)
  (with-monad M
    (do (cons σ _) ← get
      (put (cons σ θ)))))
