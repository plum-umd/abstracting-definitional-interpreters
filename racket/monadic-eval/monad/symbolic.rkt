#lang racket/unit

(require racket/set "../signatures.rkt" "../transformers.rkt")

(import)
(export monad^ menv^ mstore^ symbolic^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; monad^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M (ReaderT (FailT (StateT #f (StateT #f (NondetT ID))))))

(define (mrun m)
  ;; A path-condition is a set of symbolic values known to have evaluated to 0
  (run-StateT (set) (run-StateT (hash) (run-ReaderT (hash) m))))

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
;;;;; env^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

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
