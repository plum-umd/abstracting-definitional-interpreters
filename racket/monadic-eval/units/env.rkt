#lang racket/unit

(require "../signatures.rkt" "../../monad-transformers.rkt")

(import monad^)
(export env^)

(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))
