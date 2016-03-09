; Conclusions:
; let's support stuff like this: (memoify (dead-codify (fix (cached (traces ev)))))
; key components:
; • ev, which is an unfixed evaluator
; • unfixed evaluator transformers/functors, like cached and traces
; • using standard fix to fix an unfixed evaluator
; • top-level ev modifiers like memoify and dead-codify, which can be compose
;   (please rename these to something better)
; Use from this file: caching, dead-codify, memoify, mlfp
; Coo'

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

; memo-fix0 : ∀mev. (e → m(v)) → e → m(v)
; dead-fix0 : ∀mev. (e → m(v)) → e → m(v)

(memoify (dead-codify (fix (cached (traces ev)))))

; dead-fix : ∀mev. ((e → m(v)) → e → m(v)) → e → m(v)
(define ((dead-fix ev) e)
  (put-dead-es e)
  ((fix ev) e))

; dead-codify : ∀mev. (e → m(v)) → e → m(v)
(define ((dead-codify eval) e)
  (do
    (put-dead-es e)
    (eval e)))

; memoify : ∀mev. (e → m(v)) → e → m(v)
(define ((memoify eval) e)
  ρ ← ask-env
  σ ← get-store
  ς ≔ (list e ρ σ)
  g$ ← (mlfp (λ (g$)
                (do
                  (put-l$ (hash))
                  (put-store σ)
                  (local-gcache g$ (eval e))
                  get-l$)))
  (return (hash-ref g$ ς)))

; mlfp : ∀mkv. ((k ⇰ v) → m(k ⇰ v)) → m(k ⇰ v)
(define (mlfp f)
  (define (loop x)
    (do
      x' ← (f x)
      (if (equal? x' x)
        (return x')
        (loop x'))))
  (loop (hash)))

; notes from DD and NL discussion



; oev is called traces,
;
; to use it, write (fix (traces ev)) where ev is the standard unfixed
; interpreter.




; #lang monadic-eval (@Cδ @traces-monad @state @ev) fix
; this is instantiating:
;   δ^
;   monad^
;   env ; reader
;   store ; state
;   state ; memory interface
;   ... (like traces writer, etc.)
;   
; term
;   eval : exp → m(val)
;
;   ev^ : unfixed evaluator ((e → m v) → e → m v) → (e → m v) → e → m v
;   instantiate to:
;       standard ev
;       (traces ev)
;       etc.
;   the reason to make them a unit is because they need to see monadic effects
;   (and other stuffs), and those are units.
;
;  
;   we need to provide 
;   eval^ : e → m v
;
;   
;   
;   
; #lang monadic-eval (@Cδ @traces-monad @state) (fix (traces ev))
; #lang monadic-eval (@Cδ @traces-monad @state) (deadfix ev)
; #lang monadic-eval (@Cδ @traces-monad @state) (memofix (caching ev))


; file1:
; ev : ... unfixed guy ...
; (export/unit ev : ev^)
;
; file2:
; traces : ... unfixed → unfixed ...
; (export/unit traces : traces^)
;
; file3:
; cached : ... unfixed → unfixed ...
; (export/unit cached : cached^)
;
; file4:
; deadfixed : ... unfixed → evaluator
; (export/unit deadfix : deadfix^)
;
; file4:
; #lang (monadic-eval (@Cδ @traces-M @state1 ... @memofix @cached @traces @ev) (memofix (cached (traces ev))))
;                                                 memofix^ cached^ traces^ ev^

; #lang (monadic-eval (@Cδ @id-M @meta-state) (ev^) (fix ev))



