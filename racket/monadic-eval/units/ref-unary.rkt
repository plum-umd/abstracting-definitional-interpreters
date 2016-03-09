#lang racket/unit
(require racket/set
         "../transformers.rkt"
         "../signatures.rkt")
(import monad^ alloc^ menv^ mstore^)
(export ref^)

(define (hash-join σ a v)
  (hash-update σ a (λ (vs₀) (set-add vs₀ v)) {set v}))

;; ref^ impl

(define (mkbox v)
  ;; mkbox : value → M addr
  (with-monad M
    (do σ ← get-store
        a ← 'box ; One box per program abstraction
        (put-store (hash-join σ a v))
        (return a))))

(define (sbox a v)
  ;; sbox : addr → value → M addr
  (with-monad M
    (do σ  ← get-store
        σ* ≔ (hash-join σ a v)
        (put-store σ*)
        (return a))))

(define (ubox a)
  ;; ubox : addr → M value
  (with-monad M
    (do σ ← get-store
        (if (hash-has-key? σ a)
            (return (hash-ref σ a))
            fail))))
