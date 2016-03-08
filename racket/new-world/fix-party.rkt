; caching : ∀mev. ((e → m(v)) → e → m(v)) → (e → m(v)) → e → m(v)
(define (((caching ev) ev-caching) e)
  (do
    ρ ← ask-env
    σ ← get-store
    ς ≔ (list e ρ σ)
    l$ ← get-lcache
    (if (hash-has-key? $ ς)
      (return (hash-ref $ ς))
      (do
        g$ ← ask-gcache
        (update-lcache (λ (l$) (hash-set l$ ς (hash-ref g$ ς (set)))))
        v ← ((ev ev-caching) e)
        (update-lcache (λ (l$) (hash-union l$ (hash ς (set v)))))
        (return v)))))

; memo-fix : ∀mev. ((e → m(v)) → e → m(v)) → e → m(v)
(define ((memo-fix ev) e)
  ρ ← ask-env
  σ ← get-store
  ς ≔ (list e ρ σ)
  g$ ← (mlfp (λ (g$)
                (do
                  (put-l$ (hash))
                  (put-store σ)
                  (local-gcache g$ ((fix ev) e))
                  get-l$)))
  (return (hash-ref g$ ς)))

; dead-fix : ∀mev. ((e → m(v)) → e → m(v)) → e → m(v)
(define ((dead-fix ev) e)
  e₀ ← ask-program
  (put-dead-es e₀)
  ((fix ev) e))

; mlfp : ∀mkv. ((k ⇰ v) → m(k ⇰ v)) → m(k ⇰ v)
(define (mlfp f)
  (define (loop x)
    (do
      x' ← (f x)
      (if (equal? x' x)
        (return x')
        (loop x'))))
  (loop (hash)))
