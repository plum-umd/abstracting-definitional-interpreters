#lang racket/unit
(require "../signatures.rkt"
         "../../monad-transformers.rkt")
(import)
(export monad^ env^ store^ reach^)

(define M (ReaderT (StateT #f (WriterT PowerO (FailT ID)))))

;; mrun : M ρ σ a [-> σ] [-> ρ] -> a
(define (mrun m [σ₀ (hash)] [ρ₀ (hash)])
  (run-ReaderT ρ₀ (run-StateT σ₀ m)))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

;; store^ impl:
(define get-store (with-monad M get))
(define put-store (with-monad M put))

;; reach^ impl:
(define tell-reach   (with-monad M tell))
(define hijack-reach (with-monad M hijack))
