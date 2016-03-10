#lang racket/unit
(require racket/match
         racket/set
         "../transformers.rkt"
         "../syntax.rkt"
         "../signatures.rkt")

(import monad^ menv^ mstore^ mcached^ δ^ alloc^)
(export ev-cache^)

(define (((ev-cache ev0) ev) e)
  (with-monad M
    (do ρ ← ask-env
        σ ← get-store
        ς ≔ (list e ρ σ)
        Σ ← get-$
        (if (hash-has-key? Σ ς)
            (return (Σ ς))
            (do Σ⊥ ← ask-⊥
                Σ* ≔ (hash-set Σ ς (Σ⊥ ς {set}))
                (put-$ Σ*)
                v  ← ((ev0 ev) e)
                (update-$
                  (λ (Σ′) (hash-update Σ′ ς (λ (vs) (set-add vs v)) {set v})))
                (return v))))))
