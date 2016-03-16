#lang racket
(require racket/match
         "../map.rkt"
         "../signatures.rkt"
         "../unparse.rkt"
         "../transformers.rkt")
(provide CacheT run-CacheT ret-CacheT)

(define (CacheT O M)
  (ReaderT   ; Σ⊥
   (StateT O ; Σ
           ID)))

(define (run-CacheT Σ⊥₀ Σ₀ m)
  (run-StateT  Σ₀
  (run-ReaderT Σ⊥₀ m)))

;; disregard the cache on return
(define (ret-CacheT x) (car x))
