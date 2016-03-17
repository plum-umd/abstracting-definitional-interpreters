#lang racket/unit

(require racket/match
         "../signatures.rkt"
         "../transformers.rkt"
	 "../unparse.rkt"
	 "../map.rkt")

(import)
(export monad^ menv^ mstore^)

;; M a ≡ r → s → Option (a * s)
(define M
  (monad
   
   ;; return
   (λ (a)
     (λ (r)
       (λ (s)
         (cons a s))))
   
   ;; bind
   (λ (ma f)
     (λ (r)
       (λ (s)
         (match ((ma r) s)
           [(cons a s*)
            (((f a) r) s*)]
           [(failure) (failure)]))))

   ;; effects
   (hash
    'reader (monad-reader
             ;; ask
             (λ (r)
               (λ (s)
                 (cons r s)))
             ;; local
             (λ (r* m)
               (λ (r)
                 (λ (s)
                   ((m r*) s)))))
    'state (monad-state
            ;; get
            (λ (r)
              (λ (s)
                (cons s s)))
            
            ;; put
            (λ (s*)
              (λ (r)
                (λ (s)
                  (cons (void) s*))))
            )
    'fail (monad-fail
           ;; fail
           (λ (r)
             (λ (s)
               (failure)))
           ;; try
           (λ (m₁ m₂)
             (λ (r)
               (λ (s)
                 (match ((m₁ r) s)
                   [(failure) ((m₂ r) s)]
                   [x x]))))))

   ;; properties
   (hash)))

(define-monad M)

;; mrun : (M a) [→ ρ [→ θ [→ σ]]] → a × σ
(define (mrun m [ρ₀ ∅] [σ₀ ∅])
  ((m ρ₀) σ₀))

(define mret unparse-⟨maybe-v⟩×σ)

;; env^ impl:
(define ask-env ask)
(define local-env local)

;; store^ impl:
(define get-store get)
(define put-store put)
(define (update-store f)
  (do σ ← get-store
      (put-store (f σ))))
