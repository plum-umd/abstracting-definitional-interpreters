#lang racket/unit
(require "../map.rkt"
         racket/set
         "../transformers.rkt"
         "../signatures.rkt")

(import mcache^ menv^ mstore^ monad^)
(export eval-coind^)

(define-monad M)

; eval-coind : (e → M v) → e → M v
(define ((eval-coind eval) e)  
  (do ρ ← ask-env
      σ ← get-store
      ς ≔ (list e ρ σ)
      Σ ← (mlfp (λ (Σ) (do (put-$ ∅)
                           (put-store σ)
                           (local-⊥ Σ (local-env ρ (eval e)))
                           Σ′ ← get-$
                           (return Σ′))))
      (for/monad+ ([v (Σ ς)]) (return v))))

; mlfp : ((k → v) → M (k → v)) → M (k → v)
(define (mlfp f)
  (let loop ([x ∅])
    (do x′ ← (f x)
      (if (equal? x′ x)
          (return x′)
          (loop x′)))))
