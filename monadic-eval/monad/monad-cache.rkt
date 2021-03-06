#lang racket/unit
(require racket/match
         "../map.rkt"
         "../signatures.rkt"
	 "../unparse.rkt"
         "../transformers.rkt")
(import)
(export monad^ menv^ mstore^ mcache^)

;; M ρ σ Σ a := ρ → σ → Σ → ℘(((a ∪ (failure)) × σ)) × Σ
(define M
  (ReaderT                  ; ρ
    (FailT
   (StateT #f               ; σ
  (NondetT
   (StateT (FinMapO PowerO) ; Σ
        ID))))))

(define-monad M)

(define (mrun m [ρ₀ ∅] [σ₀ ∅] [Σ₀ ∅])
  (run-StateT  Σ₀
  (run-StateT  σ₀
  (run-ReaderT ρ₀ m))))

(define (mret x)
  (match x ; disarcd cache and store
    [(cons svs cache) (unparse-⟨⟨maybe-v⟩×σ⟩set/discard-σ svs)]))

(define ask-⊥ (lambda _ (error "unimplemented")))
(define local-⊥ (lambda _ (error "unimplemented")))

;; menv^ impl:

(define ask-env ask)

(define local-env local)


;; mstore^ impl:

(define get-store (bind get (compose1 return car)))

(define (put-store σ)
  (do (cons _ Σ) ← get
    (put (cons σ Σ))))

(define (update-store f)
  (do σ ← get-store
    (put-store (f σ))))


;; mcache^ impl:

(define get-$ (bind get (compose1 return cdr)))
(define (put-$ Σ)
  (do (cons σ _) ← get
    (put (cons σ Σ))))
(define (update-$ f)
  (do Σ ← get-$
    (put-$ (f Σ))))
