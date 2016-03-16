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
      (mlfp (λ (Σ) (do (put-$ ∅)
                       (put-store σ)
                       (local-⊥ Σ (eval e))
                       get-$)))
      Σ ← get-$
      (for/monad+ ([v.σ (Σ ς)])
        (do (put-store (cdr v.σ))
            (return (car v.σ))))))

; mlfp : ((k → v) → M (k ↦ v)) → M unit
(define (mlfp f)
  (let loop ([x ∅])
    (do x′ ← (f x)
      (if (equal? x′ x)
          (return (void))
          (loop x′)))))
