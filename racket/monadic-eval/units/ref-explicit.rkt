#lang racket/unit
(require "../transformers.rkt"
         "../signatures.rkt")
(import monad^ alloc^ menv^ mstore^)
(export ref^)

;; ref^ impl
;; assumes a monad stack that includes FailT

(define (mkbox v)
  ;; mkbox : value → M addr
  (with-monad M
    (do σ  ← get-store
        a  ← (alloc v)
        σ* ≔ (hash-set σ a v)
      (put-store σ*)
      (return a))))

(define (sbox a v)
  ;; sbox : addr → value → M addr
  (with-monad M
    (do σ  ← get-store
        σ* ≔ (hash-set σ a v)
      (put-store σ*)
      (return a))))

(define (ubox a)
  ;; sbox : addr → M value
  (with-monad M
    (do σ ← get-store
      (if (hash-has-key? σ a)
          (return (hash-ref σ a))
          fail))))
