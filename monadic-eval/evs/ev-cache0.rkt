#lang racket/unit
(require racket/match
         racket/set
         "../map.rkt"
         "../transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^ mcache^ δ^ alloc^)
(export ev-cache^)
(init-depend monad^)

(define-monad M)

(define (((ev-cache ev₀) ev) e)
  (do ρ ← ask-env
      σ ← get-store
      ς ≔ (list e ρ σ)
      Σ ← get-$
      (if (∈ ς Σ)
          (for/monad+ ([v.σ (Σ ς)])
            (do (put-store (cdr v.σ))
                (return (car v.σ))))
          (do (put-$ (Σ ς (set)))
              v  ← ((ev₀ ev) e)
              (update-$ (λ (Σ) (Σ ς (set-add (Σ ς) (cons v σ)))))
              (return v)))))
