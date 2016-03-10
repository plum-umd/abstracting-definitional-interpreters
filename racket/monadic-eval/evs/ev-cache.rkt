#lang racket/unit
(require racket/match
         racket/set
         "../map.rkt"
         "../transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^ mcache^ δ^ alloc^)
(export ev-cache^)

(define (((ev-cache ev0) ev) e)
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
        ς ≔ (list e ρ σ)
        Σ ← get-$
        (if (ς . ∈ . Σ)
            (for/monad+ ([v (Σ ς)]) (return v))
            (do Σ⊥ ← ask-⊥
                Σ* ≔ (Σ ς (if (ς . ∈ . Σ⊥) (Σ⊥ ς) {set}))
                (put-$ Σ*)
                v ← ((ev0 ev) e)
                (update-$ (λ (Σ′) (Σ′ ς (set-add (Σ′ ς) v))))
                (return v))))))
