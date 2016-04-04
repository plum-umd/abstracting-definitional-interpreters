#lang racket/unit

(require racket/match
         racket/set
         "../unparse.rkt"
         "../signatures.rkt"
         "../syntax.rkt"
         "../transformers.rkt"
         "../map.rkt")

(import state^)
(export ev-loop^ alloc^ monad^ menv^ mstore^ mhistory^ history^)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ev-loop^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (((ev-loop ev₀) ev) e)
  (with-monad M
    (match e
      [(app e₀ e₁)
       (do
         (cons (lam x e*) ρ) ← (ev e₀)
         vₓ                  ← (ev e₁)
         (local-call (list e₀ e₁ e*) ; push source `(e₀ e₁)` and target `e*`
           (do a ← (alloc x) ; important to call alloc *inside* push
               (ext a vₓ)
               (local-env (ρ x a) (ev e*)))))]
      [_ ((ev₀ ev) e)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; alloc^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alloc x)
  (with-monad M
    (do H ← ask-call-history
      #|
        _ ≔ (begin
              (printf "context for ~a:~n" x)
              (for ([c (in-set (H⁻¹ H))])
                (match-define (list e₀ e₁ e) c)
                (printf "  - ~a ~a -> ~a~n" (unparse e₀) (unparse e₁) (unparse e)))
              (printf "~n"))
      |#
        (return (cons x H)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; monad^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M (ReaderT
            (FailT
           (StateT #f
          (NondetT
          (ReaderT ID))))))

(define (mrun m)
  (run-ReaderT H∅ (run-StateT ∅ (run-ReaderT ∅ m))))

(define (mret xs) #|TODO|# xs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mstore^ (standard stuff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-store (with-monad M get))
(define put-store (with-monad M put))
(define (update-store f)
  (with-monad M
    (do σ ← get-store
        (put-store (f σ)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; menv^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ask-env
  (with-monad M
    (do (cons ρ _) ← ask
        (return ρ))))

(define (local-env ρ m)
  (with-monad M
    (do (cons _ H) ← ask
        (local (cons ρ H) m))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mhistory^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ask-call-history
  (with-monad M
    (do (cons _ H) ← ask
        (return H))))

(define (local-call c m)
  (with-monad M
    (do (cons ρ H) ← ask
        H* ≔ (H+ H c)
        (local (cons ρ H*) m))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; history^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Represent each set as an integer, with empty set `H∅` and set addition `H+`.
;; This is not essential, and is purely an optimization and output clean-up.
(define-values (H∅ H+ H⁻¹)
  (let ([m   (make-hash  )]
        [m⁻¹ (make-hasheq)])

    ;; Assign the next integer to an unseen set, or give an old integer for a seen set
    (define (encode s)
      (hash-ref! m s (λ ()
                       (define i (hash-count m))
                       (hash-set! m⁻¹ i s)
                       i)))

    ;; Return the set represented by given integer
    (define (decode i)
      (hash-ref m⁻¹ i (λ () (error 'decode "Nothing corresponds to ~a" i))))

    (values
     ;; Representation of empty set
     (encode {set})
     
     ;; Return representation of set `H` with extra element `c`
     (λ (H c) (encode (set-add (decode H) c)))
     
     ;; Return set encoded by `H`. This is just for debugging
     decode)))
