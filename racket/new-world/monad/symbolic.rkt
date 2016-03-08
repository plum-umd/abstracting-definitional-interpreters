#lang racket/unit

(require racket/set "../signatures.rkt" "../../monad-transformers.rkt")

(import)
(export monad^ store^ symbolic^)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; monad^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M (StateT PowerO (ReaderT (NondetT ID))))

(define (mrun m)
  ;; Path condition represented as set of evaluation known to have evaluated to 0
  (run-StateT (hash) (run-ReaderT (cons (hash) (set)) m)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; store^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-store
  (with-monad M
    (do (cons σ _) ← get
        (return σ))))

(define (put-store σ*)
  (with-monad M
    (do (cons σ φ) ← get
        (put (cons σ* φ)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; symbolic^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbolic? x)
  (or (symbol? x) (pair? x)))

(define (refine e)
  (with-monad M
    (do (cons σ φ) ← get
        (put (cons σ (set-add φ e))))))

(define get-path-cond
  (with-monad M
    (do (cons _ φ) ← get
        (return φ))))
