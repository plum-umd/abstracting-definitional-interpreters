#lang racket/unit
(require racket/set
         "../signatures.rkt"
         "../../monad-transformers.rkt")
(import)
(export monad^ env^ store^ dead^)

(define M (ReaderT (StateT #f (StateT #f (FailT ID)))))

;; mrun : M ρ σ a → v
(define (mrun m)
  (run-StateT (hash) (run-StateT {set} (run-ReaderT (hash) m))))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

;; store^ impl:
(define get-store (with-monad M (bind get (compose1 return cdr))))
(define (put-store σ)
  (with-monad M
    (do (cons θ _) ← get
      (put (cons θ σ)))))

;; dead^ impl:
(define get-dead (with-monad M (bind get (compose1 return car))))
(define (put-dead θ)
  (with-monad M
    (do (cons _ σ) ← get
      (put (cons θ σ)))))
