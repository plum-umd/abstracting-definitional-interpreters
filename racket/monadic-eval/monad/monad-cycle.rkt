#lang racket/unit
(require racket/set
         "../signatures.rkt"
         "../transformers.rkt"
         "../map.rkt")
(import)
(export monad^ menv^ mstore^ mcycle^)

;; M ρ σ a := ρ → σ → a × σ
(define M
  (ReaderT
    (FailT
   (StateT #f
  (NondetT
  (ReaderT ID))))))

(define-monad M)

;; mrun : (M ρ σ Σ a) [→ ρ [→ σ [→ Σ]]] → a × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅] [Σ₀ {set}])
  (run-ReaderT Σ₀ (run-StateT σ₀ (run-ReaderT ρ₀ m))))

(define (mret x) x)

;; env^ impl:
(define ask-env (bind ask (compose1 return car)))
(define (local-env ρ m)
  (do (cons _ Σ) ← ask
    (local (cons ρ Σ) m)))

;; store^ impl:
(define get-store (with-monad M get))
(define put-store (with-monad M put))
(define (update-store f)
  (with-monad M
    (do σ ← get-store
        (put-store (f σ)))))

;; cycle^ impl:
(define ask-cycle (bind ask (compose1 return cdr)))
(define (local-cycle Σ m)
  (do (cons ρ _) ← ask
    (local (cons ρ Σ) m)))
