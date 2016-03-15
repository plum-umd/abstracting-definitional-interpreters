#lang racket/unit

(require racket/match
         racket/set 
         "../signatures.rkt" 
         "../transformers.rkt"
         "../map.rkt"
         "../unparse.rkt")

(import)
(export monad^ menv^ mstore^ msymbolic^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; monad^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M (ReaderT (FailT (StateT #f (StateT #f (NondetT ID))))))

(define (mrun m)
  ;; A pathdition is a set of symbolic values known to have evaluated to 0
  (run-StateT (set) (run-StateT ∅ (run-ReaderT ∅ m))))

;; placeholder
(define (mret xs)
  (for/set ([x xs])
    (match-define (cons (cons e _) φ) x)
    `(,e . ,φ)))

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
(define (update-store f)
  (with-monad M
    (do
      σ ← get-store
      (put-store (f σ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; env^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ask-env   (with-monad M ask))
(define local-env (with-monad M local))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; symbolic^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (refine e)
  (with-monad M
    (do (cons σ φ) ← get
        (put (cons σ (set-add φ e))))))

(define get-path-cond
  (with-monad M
    (do (cons _ φ) ← get
        (return φ))))
