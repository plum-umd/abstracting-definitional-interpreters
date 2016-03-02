#lang racket/unit
(require racket/match
         "../signatures.rkt"
         "../subexp.rkt")
(import)
(export monad^)

;; FIXME: can't write mrun here because need expr.
(define (mrun M) (error "Can't run"))

;; Same as trace monad
(define (((return v) s) t) (cons (cons v s) t))
(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t) (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
