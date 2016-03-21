#lang racket/unit
(require "../set.rkt"
         "../map.rkt"
         "../syntax.rkt"
         "../signatures.rkt"
         "../transformers.rkt"
         racket/match)
(import monad^ mlive^ menv^ mstore^ state^)
(export gc-count^)

(define-monad M)

;; We can't just walk the stack, because the stack doesn't exist in a
;; way we can walk directly. What do we get when we walk the stack?
;; - the es yet to be evaluated in κs
;; - the addrs live in κs
;; This is in addition to what we get from e, ρ, and σ.
;;
;; So, we need to know which es may be evaluated so we know which
;; variables may be needed. When are we allowed to stop caring about
;; an arbitrary e? For e₀, we care about all the xs (but they don't
;; point to any αs yet). So, we collect all of the free xs in e₀, and
;; when we stop caring about each one, we ignore it.
;;
;; when do we stop caring about x?
;; 
;; We can stop caring about x when we've evaluated all instances of a
;; that particular x or we've finished evaluating all es that contain
;; instances of x as a sub e. We can't do this lexically without know
;; order of operations, so we carry addr reference counts in an
;; additional StateT which must be store-specific.

(define-syntax define/memo
  (syntax-rules ()
    [(_ (name arg) def+exp ...)
     (define name
       (let ([memotbl (make-weak-hasheqv)])
         (λ (arg)
           (if (hash-has-key? memotbl arg)
               (ephemeron-value (hash-ref memotbl arg))
               (let ([ans (let () def+exp ...)])
                 (hash-set! memotbl arg (make-ephemeron arg ans))
                 ans)))))]
    [(_ (name arg ...) def+exp ...)
     (define name
       (let ([memotbl (make-weak-hasheqv)])
         (λ (arg ...)
           (let ([key (list arg ...)])
             (if (hash-has-key? memotbl key)
                 (ephemeron-value (hash-ref memotbl key))
                 (let ([ans (let () def+exp ...)])
                   (hash-set! memotbl key (make-ephemeron key ans))
                   ans))))))]))

(define inc +)
(define dec -)

(define/memo (count-vars e)
  (match e
    [(app e0 e1)    (⊔ (count-vars e0) (count-vars e1) #:combine inc)]
    [(lam x e)      (map-rem (count-vars e) x)]
    [(vbl x)        (∅ x 1)]
    [(ifz e0 e1 e2) (⊔ (count-vars e0) (count-vars e1) (count-vars e2)
                       #:combine inc)]
    [(op1 o e)      (count-vars e)]
    [(op2 o e0 e1)  (⊔ (count-vars e0) (count-vars e1)
                       #:combine inc)]
    [(ref e)        (count-vars e)]
    [(drf e)        (count-vars e)]
    [(srf e0 e1)    (⊔ (count-vars e0) (count-vars e1)
                       #:combine inc)]
    [(sym s)        ∅]
    [(lrc f e0 e1)  (map-rem (⊔ (count-vars e0) (count-vars e1)
                               #:combine inc)
                            f)]
    [_              ∅]))

(define (count-addrs ρ e)
  (for/map ([(x n) (∈ (count-vars e))] #:when (∈ x ρ))
    (values (ρ x) n)))

;; searches for live addrs from the initial αₑ
(define (live αₑ [α ∅] [seen {set}])
  (define a×n (for/first ([(a n) (∈ αₑ)]) (cons a n)))
  (if (not a×n)
      (return α)
      (let* ([a   (car a×n)]
             [n   (cdr a×n)]
             [αₑ′ (map-rem αₑ a)])
        (if (set-member? seen a)
            (live αₑ′ α seen)
            (let ([seen′ (set-add seen a)])
              (do α′ ← (live αₑ′ (α a (inc (if (∈ a α) (α a) 0) n)) seen′)
                  v  ← (find a)
                  (match v
                    [(cons e ρ)    (live (count-addrs ρ e) α′ seen′)]
                    [(cons 'box a) (live (∅ a 1) α′ seen′)]
                    [_             (return α′)])))))))

;; counts live vars in e, incrementing live addr counts
(define (count e m)
  (do ρ  ← ask-env
      α  ← get-live
      αₑ ≔ (count-addrs ρ e)
      α′ ← (live αₑ α)
      (put-live α′)
      ρ′ ≔ (for/map ([(x a) (∈ ρ)] #:when (∈ a α′)) (values x a))
      (local-env ρ′ m)))

;; gc is called directly after e is popped
;; iterates through the live addrs for e, collecting
;; any dead after decrementing the count
(define (gc e)
  (do σ  ← get-store
      α  ← get-live
      ρ  ← ask-env
      αₑ ← (live (count-addrs ρ e))
      α′ ≔ (⊔ α αₑ #:combine dec)
      aₙ ≔ (for/set ([(a n) (∈ α′)] #:when (not (eqv? n 0))) a)
      (put-live  (restrict α′ aₙ))
      (put-store (restrict σ  aₙ)))) 
