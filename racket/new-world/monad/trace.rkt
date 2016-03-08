#lang racket/unit
(require "../signatures.rkt"
         "../../monad-transformers.rkt")
(import)
(export monad^ env^ store^ trace^)

(define M (ReaderT (StateT #f (WriterT ListO (FailT ID)))))

;; mrun : M ρ σ a [-> ρ] [-> σ] -> a
(define (mrun m [ρ₀ (hash)] [σ₀ (hash)])
  (run-ReaderT ρ₀ (run-StateT σ₀ m)))

;; env^ impl:
(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

;; store^ impl:
(define get-store (with-monad M get))
(define put-store (with-monad M put))

;; trace^ impl:
(define tell-trace   (with-monad M tell))
(define hijack-trace (with-monad M hijack))
