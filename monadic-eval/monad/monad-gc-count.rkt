#lang racket/unit
(require racket/match
         racket/set
         "../map.rkt"
         "../signatures.rkt"
         "../unparse.rkt"
         "../transformers.rkt")
(import)
(export monad^ menv^ mstore^ mlive^)

;; M ρ α σ a := ρ → α → σ → (a × σ)
(define M
  (ReaderT                  ; ρ
   (StateT #f               ; α
    (FailT
   (StateT #f               ; σ
           ID)))))

(define-monad M)

(define (mrun m [ρ₀ ∅] [α₀ ∅] [σ₀ ∅])
  (run-StateT  σ₀
  (run-ReaderT α₀
  (run-ReaderT ρ₀ m))))

(define (mret x) x)

;; menv^ impl:

(define ask-env ask)

(define local-env local)

;; mstore^ impl:
(define get-store (bind get (compose1 return cdr)))

(define (put-store σ)
  (do `(,α . ,_) ← get
    (put `(,α . ,σ)))) 

(define (update-store f)
  (do σ ← get-store
    (put-store (f σ))))

;; mlive^ impl:

(define get-live (bind get (compose1 return car)))

(define (put-live α)
  (do (cons _ σ) ← get
    (put (cons α σ))))

(define (update-live f)
  (do θ ← get-live
      (put-live (f θ))))
