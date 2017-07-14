#lang racket/unit
(require racket/set
         "../signatures.rkt"
         "../transformers.rkt"
	 "../unparse.rkt"
	 "../map.rkt")
(import)
(export monad^ menv^ mstore^ gc^)

;; monad^ impl:

(define M (ReaderT (ReaderT (FailT (StateT #f ID)))))
(define-monad M)

;; mrun : (M a) [→ ρ [→ θ [→ σ]]] → a × σ
(define (mrun m [ρ₀ ∅] [ψ₀ (set)] [σ₀ ∅])
  (run-StateT σ₀ (run-ReaderT ρ₀ (run-ReaderT ψ₀ m)))) ; not sure about order

(define (mret x) x)

;; gc^ impl:
(define ask-roots
  (bind ask (compose1 return car)))

(define (extra-roots rs m)
  (do (cons ψ ρ) ← ask
    (local (cons (set-union ψ rs) ρ) m)))

;; env^ impl:
(define ask-env
  (bind ask (compose1 return cdr)))

(define (local-env ρ m)
    (do (cons ψ _) ← ask
      (local (cons ψ ρ) m)))

;; store^ impl:
(define get-store get)
(define put-store put)
(define (update-store f)
  (do σ ← get-store
      (put-store (f σ))))


