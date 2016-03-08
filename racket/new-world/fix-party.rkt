(define (((caching ev) ev-caching) e)
  (do
    ρ ← ask-env
    σ ← get-store
    ς ≔ (list e ρ σ)
    l$ ← get-l$
    (if (hash-has-key? $ ς)
      (return (hash-ref $ ς))
      (do
        g$ ← ask-g$
        (update-l$ (λ (l$) (hash-set l$ ς (hash-ref g$ ς (set)))))
        v ← ((ev ev-caching) e)
        (update-l$ (λ (l$) (hash-union l$ (hash ς (set v)))))
        (return v)))))

(define ((memo-fix ev) e)
  ρ ← ask-env
  σ ← get-store
  ς ≔ (list e ρ σ)
  g$ ← (mlfp (λ (g$)
                (do
                  (put-l$ (hash))
                  (put-store σ)
                  (local-g$ g$ ((fix ev) e))
                  get-l$)))
  (return (hash-ref g$ ς)))

(define ((dead-fix ev) e)
  e₀ ← ask-program
  (put-dead-es e₀)
  ((fix ev) e))

(define (mlfp f) ; f : a → m(a)
  (define (loop x)
    (do
      x' ← (f x)
      (if (equal? x' x)
        (return x')
        (loop x'))))
  (loop (hash)))
