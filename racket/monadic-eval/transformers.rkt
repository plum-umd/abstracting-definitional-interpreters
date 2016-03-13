#lang racket

(provide (all-defined-out))

(require (for-syntax racket/syntax syntax/parse)
         rackunit
         racket/hash
         "map.rkt")

;;;;;;;;;;
; Monoid ;
;;;;;;;;;;

; A monoid is a collection of ozero+oplus operations. ozero is a unit and oplus
; is associative and sometimes commutative.
; monoid (O : type) : type
; ozero : O
; oplus : O,O → O
(struct monoid (ozero oplus) #:transparent)

(define ((monoid-oconcat O) os)
  (sequence-fold (monoid-oplus O) (monoid-ozero O) os))

;;;;;;;;;
; Monad ;
;;;;;;;;;

; A monad is a collection of return+bind operations and a map from supported
; effect names to their implementations. Monad transformers will propagate as
; many effect operations as they know how. The properties field holds
; properties that another monad might need to lift an effect. The only property
; used here is being a monoid functor, which NondetT requires.

; monad (M : type → type) : type
; return : ∀a, a → M(a)
; bind : ∀ab, M(a),(a → M(b)) → M(b)
; effects : effect-name ⇰ effect-for(effect-name)
; effect-name ∈ {reader, writer, state, fail, nondet}
; effect-for : effect-name → type
(struct monad (return bind effects properties) #:transparent)

;;;;;;;;;;;
; Effects ;
;;;;;;;;;;;

; Effects are operations supported by some monad

; effect-for(reader) = monad-reader
; monad-reader (r : type) (M : type → type) : type
; ask : M(r)
; with : ∀a, r,M(a) → M(a)
; local(r,ask) = return(r)
(struct monad-reader (ask local) #:transparent)

; effect-for(writer) = monad-writer
; monad-writer (o : type) (M : type → type) : type
; tell : o → M(unit)
; hijack : ∀a, M(a) → M(a,o)
; hijack(tell(o)) = return(unit,o)
(struct monad-writer (tell hijack) #:transparent)

; effect-for(state) = monad-state
; monad-state (s : type) (M : type → type) : type
; get : M(s)
; put : s → M(unit)
; x ← get ; put(x) = return(unit)
(struct monad-state (get put) #:transparent)

; effect-for(fail) = monad-fail
; monad-fail (M : type → type) : type
; fail : ∀(a), M(a)
; try : ∀(a), M(a),M(a) → M(a)
; try(fail,xM) = xM
; try(return(x),xM) = return(x)
(struct monad-fail (fail try) #:transparent)

; effect-for(nondet) = monad-nondet
; nondet (M : type → type) : type
; mzero : ∀a, M(a)
; mplus : ∀a, M(a),M(a) → M(a)
; mplus(mzero,xM) = xM
 ; mplus(xM,mzero) = xM
(struct monad-nondet (mzero mplus) #:transparent)

;;;;;;;;;;
; Syntax ;
;;;;;;;;;;

; `(with-monoid O e)` introduces `ozero`, `oplus` and `oconcat` specialized to
; the monoid `O` into scope inside of `e`.
(define-syntax (define-monoid stx)
  (syntax-parse stx
    [(define-monoid O)
     (with-syntax ([ozero (format-id #'O "ozero")]
                   [oplus (format-id #'O "oplus")]
                   [oconcat (format-id #'O "oconcat")])
       #'(begin
           (define O′ O)
           (match-define (monoid ozero oplus) O′)
           (define oconcat (monoid-oconcat O′))))]))

(define-syntax-rule
  (with-monoid O e)
  (let () (define-monoid O) e))
; `(with-monad M e)` introduces monad operations and do notation into scope
; inside of `e`. There might be a better way to do this (or at least automate
; it) but I have bigger fish to fry at the moment.

(define-syntax (define-monad stx)
  (syntax-parse stx
    [(define-monad M)
     (with-syntax ([return (format-id #'M "return")]
                   [bind (format-id #'M "bind")]
                   [do (format-id #'M "do")]
                   [for/monad+ (format-id #'M "for/monad+")]
                   [ask (format-id #'M "ask")]
                   [local (format-id #'M "local")]
                   [tell (format-id #'M "tell")]
                   [hijack (format-id #'M "hijack")]
                   [get (format-id #'M "get")]
                   [put (format-id #'M "put")]
                   [fail (format-id #'M "fail")]
                   [try (format-id #'M "try")]
                   [mzero (format-id #'M "mzero")]
                   [mplus (format-id #'M "mplus")]
                   [monoid-functor (format-id #'M "monoid-functor")])
       #'(begin
           (define M′ M)
           (match-define (monad return bind effects properties) M′)
           (define-syntax do
             (syntax-rules (← ≔)
               [(do xM) xM]
               [(do p ← xM . bs)
                (bind xM (match-lambda
                           [p (do . bs)]))]
               [(do x ≔ b . bs)
                (match-let ([x b])
                  (do . bs))]
               [(do xM . bs)
                (bind xM (λ (x)
                           (do . bs)))]))
           (define monoid-functor (hash-ref properties 'monoid-functor #f))
           (match-define (monad-reader ask local) (hash-ref effects 'reader (monad-reader #f #f)))
           (match-define (monad-writer tell hijack) (hash-ref effects 'writer (monad-writer #f #f)))
           (match-define (monad-state get put) (hash-ref effects 'state  (monad-state #f #f)))
           (match-define (monad-fail fail try) (hash-ref effects 'fail   (monad-fail #f #f)))
           (match-define (monad-nondet mzero mplus) (hash-ref effects 'nondet (monad-nondet #f #f)))
           (define-syntax (for/monad+ stx)
             (syntax-case stx ()
               [(_ clauses . defs+exprs)
                (with-syntax ([original stx])
                  #'(for/fold/derived original
                                      ([m mzero])
                                      clauses
                                      (mplus m (let () . defs+exprs))))]))))]))

(define-syntax-rule
  (with-monad M e)
  (let () (define-monad M) e))


;;;;;;;;;;;;;
; Instances ;
;;;;;;;;;;;;;

; Addition Commutative Monoid
(define AddO
  (monoid 
    ; ozero
    0 
    ; oplus
    +))

; List Monoid
(define ListO
  (monoid
    ; ozero
    '()
    ; oplus
    append))

; Max Idempotent Commutative Monoid
(define MaxO
  (monoid
    ; ozero
    #f
    ; oplus
    (λ (n₁ n₂)
       (match (cons n₁ n₂)
         [(cons #f n) n]
         [(cons n #f) n]
         [(cons n₁ n₂) (max n₁ n₂)]))))

; Powerset Idempotent Commutative Monoid
(define PowerO
  (monoid
    ; ozero
    (set)
    ; oplus
    set-union))

; Finite Map Idempotent Commutative Monoid
(define (FinMapO O)
  (monoid
   ; ozero
   ∅
   ; oplus
   (lambda (m1 m2) (⊔ m1 m2 #:combine (with-monoid O oplus)))))

; Pair Monoid
(define (PairO O₁ O₂)
  (monoid
    ; ozero
    (cons (with-monoid O₁ ozero) (with-monoid O₂ ozero))
    ; oplus
    (λ (oo₁ oo₂)
       (match-let ([(cons o₁₁ o₂₁) oo₁]
                   [(cons o₁₂ o₂₂) oo₂])
         (cons (with-monoid O₁ (oplus o₁₁ o₁₂)) (with-monoid O₂ (oplus o₂₁ o₂₂)))))))


;;;;;;;;;;;;;;;;;;
; Identity monad ;
;;;;;;;;;;;;;;;;;;

; ID a ≔ a
(define ID (monad
             (λ (x) x)
             (λ (xM f) (f xM))
             (hash)
             (hash 'monoid-functor (λ (O) O))))

(module+ test
  (check-equal?
    (with-monad ID
      (do
        x ← (return 1)
        y ≔ (+ x 1)
        z ← (return y)
        (return (+ z 1))))
    3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reader Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ReaderT r m a ≔ r → m(a)
(define (run-ReaderT r xM)
  (xM r))
(define (ReaderT M)
  (with-monad M
    (monad
      ; return
      (λ (x)
         (λ (r)
            (return x)))
      ; bind
      (λ (xM f)
         (λ (r)
            (do 
              x ← (xM r)
              ((f x) r))))
      ; effects
      ((compose
         (if (not (hash-has-key? (monad-effects M) 'reader))
           ; the standard reader effect
           (λ (h) (hash-set h 'reader (monad-reader
                                         ; ask
                                         (λ (r)
                                            (return r))
                                         ; local
                                         (λ (r′ xM)
                                            (λ (r)
                                               (xM r′))))))
           ; combining with an underlying reader effect, resulting in a reader
           ; effect of pairs: (ReaderT r₁ ∘ ReaderT r₂) = ReaderT (r₁,r₂)
           (λ (h) (hash-set h 'reader (monad-reader
                                         ; ask
                                         (λ (r)
                                            (do
                                              rₘ ← ask
                                              (return (cons r rₘ))))
                                         ; local
                                         (λ (rrₘ′ xM)
                                            (λ (r)
                                               (match-let ([(cons r′ rₘ′) rrₘ′])
                                                 (local rₘ′ (xM r′)))))))))
         (if (not (hash-has-key? (monad-effects M) 'writer)) (λ (h) h)
           ; propagating writer effects
           (λ (h) (hash-set h 'writer (monad-writer
                                         ; tell
                                         (λ (o)
                                           (λ (r)
                                             (tell o)))
                                         ; hijack
                                         (λ (xM)
                                            (λ (r) (hijack (xM r))))))))
         (if (not (hash-has-key? (monad-effects M) 'state)) (λ (h) h)
           ; propagating state effects
           (λ (h) (hash-set h 'state (monad-state
                                        ; get
                                        (λ (r)
                                           get)
                                        ; put
                                        (λ (s)
                                           (λ (r)
                                              (put s)))))))
         (if (not (hash-has-key? (monad-effects M) 'fail)) (λ (h) h)
           ; propagating fail effects
           (λ (h) (hash-set h 'fail (monad-fail
                                         ; fail
                                         (λ (r) fail)
                                         ; try
                                         (λ (xM₁ xM₂)
                                            (λ (r) (try (xM₁ r) (xM₂ r))))))))
         (if (not (hash-has-key? (monad-effects M) 'nondet)) (λ (h) h)
           ; propagating nondet effects
           (λ (h) (hash-set h 'nondet (monad-nondet
                                         ; mzero
                                         (λ (r) mzero)
                                         ; mplus
                                         (λ (xM₁ xM₂)
                                            (λ (r) (mplus (xM₁ r) (xM₂ r)))))))))
       (hash))
      ; properties
      ((compose
         (if (not (hash-has-key? (monad-properties M) 'monoid-functor)) (λ (h) h)
           ; propagating monoid functor
           (λ (h) (hash-set h 'monoid-functor (λ (O)
                                                (with-monoid (monoid-functor O)
                                                  (monoid
                                                    ; ozero
                                                    (λ (r) ozero)
                                                    ; oplus
                                                    (λ (xM₁ xM₂)
                                                       (λ (r)
                                                          (oplus (xM₁ r) (xM₂ r)))))))))))

       (hash)))))

(module+ test
  ; ReaderT(ID)(a) = r → a
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT ID)
        (do
          x ← ask
          y ← (local 10 ask)
          (return (+ x y)))))
    11)
  ; ReaderT(ReaderT(ID))(a) = r₁ → r₂ → a
  (check-equal?
    (run-ReaderT 1
      (run-ReaderT 2
        (with-monad (ReaderT (ReaderT ID))
          (do
            (cons x₁ y₁) ← ask
            (cons x₂ y₂) ← (local (cons 3 4) ask)
            (return (+ (* x₁ x₁) y₁ x₂ y₂))))))
    12)
  ; ReaderT(WriterT(ID))(a) = r → (a,o)
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT (WriterT AddO ID))
        (do
          x ← ask ;  x = 1
          (hijack 
            (local 10
              (do
                y ← ask ; y = 10
                (tell x) ; output = 1
                (tell y))))))) ; output = 11
    (cons (cons (void) 11) 0))
  ; ReaderT(StateT(ID))(a) = r → s → (a,s)
  (check-equal?
    (run-StateT 10
      (run-ReaderT 1
        (with-monad (ReaderT (StateT #f ID))
          (do
            x ← ask
            y ← get
            z ← (local 3 ask)
            (put 100)
            (return (+ x y z))))))
    (cons 14 100))
  ; ReaderT(FailT(ID))(a) = r → a+1
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT (FailT ID))
        (try
          (do
            x ← ask
            fail)
          (local 2 ask))))
    2)
  ; ReaderT(NondetT(ID))(a) = r → ℘(a)
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT (NondetT ID))
        (do
          x ← (mplus (mplus (local 5 ask) (return 2)) mzero)
          y ← ask
          (return (+ x y)))))
    (set 6 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Writer Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; WriterT o m a ≔ m(a,o)
(define (WriterT O M)
  (with-monad M
    (with-monoid O
      (monad
        ; return
        (λ (x)
           (return (cons x ozero)))
        ; bind
        (λ (xM f)
           (do
             (cons x o′) ← xM
             (cons y o″) ← (f x)
             (return (cons y (oplus o′ o″)))))
        ; effects
        ((compose
           (if (not (hash-has-key? (monad-effects M) 'reader)) (λ (h) h)
             ; propagating reader effects
             (λ (h) (hash-set h 'reader (monad-reader
                                           ; ask
                                           (do
                                             r ← ask
                                             (return (cons r ozero)))
                                           ; local
                                           (λ (r xM)
                                              (local r xM))))))
           (if (not (hash-has-key? (monad-effects M) 'writer))
             ; the standard writer effect
             (λ (h) (hash-set h 'writer (monad-writer
                                           ; tell
                                           (λ (o)
                                              (return (cons (void) o)))
                                           ; hijack
                                           (λ (xM)
                                              (do
                                                (cons x o) ← xM
                                                (return (cons (cons x o) ozero)))))))
             ; combining with an underlying writer effect, resulting in a
             ; writer effect of pairs: (WriterT o₁ ∘ WriterT o₂) = WriterT (o₁,o₂)
             (λ (h) (hash-set h 'writer (monad-writer
                                           ; tell
                                           (λ (ooₘ)
                                              (match-let ([(cons o oₘ) ooₘ])
                                                (do
                                                  (tell oₘ)
                                                  (return (cons (void) o)))))
                                           ; hijack
                                           (λ (xM)
                                              (do
                                                (cons (cons x o) oₘ) ← (hijack xM)
                                                (return (cons (cons x (cons o oₘ)) ozero))))))))
           (if (not (hash-has-key? (monad-effects M) 'state)) (λ (h) h)
             ; propagating state effects
             (λ (h) (hash-set h 'state (monad-state
                                          ; get
                                          (do
                                            s ← get
                                            (return (cons s ozero)))
                                          ; put
                                          (λ (s)
                                             (do
                                               (put s)
                                               (return (cons (void) ozero))))))))
           (if (not (hash-has-key? (monad-effects M) 'fail)) (λ (h) h)
             ; propagating fail effects
             (λ (h) (hash-set h 'fail (monad-fail
                                           ; fail
                                           fail
                                           ; try
                                           try))))
           (if (not (hash-has-key? (monad-effects M) 'nondet)) (λ (h) h)
             ; propagating nondet effects
             (λ (h) (hash-set h 'nondet (monad-nondet
                                           ; mzero
                                           mzero
                                           ; mplus
                                           mplus)))))
         (hash))
        ; properties
        ((compose
           (if (not (hash-has-key? (monad-properties M) 'monoid-functor)) (λ (h) h)
             ; propagating monoid functor
             (λ (h) (hash-set h 'monoid-functor (λ (O′) (monoid-functor (PairO O′ O)))))))
         (hash))))))

(module+ test
  ; WriterT(ID)(a) = (a,o)
  (check-equal?
    (with-monad (WriterT ListO ID)
      (do (tell (list 1))
          (tell (list 2))
        (return 5)))
    (cons 5 (list 1 2)))
  ; WriterT(ID)(a) = (a,o)
  (check-equal?
    (with-monad (WriterT AddO ID)
      (do
        (cons _ n) ← (hijack
                       (do
                         (tell 1)
                         (tell 2)))
        (tell 10)
        (tell 1)
        (return n)))
    (cons 3 11))
  ; WriterT(ReaderT(ID)(a) = r → (a,o)
  (check-equal?
    (run-ReaderT 5
      (with-monad (WriterT AddO (ReaderT ID))
        (do
          x ← ask
          (tell x)
          (return (+ x 1)))))
    (cons 6 5))
  ; WriterT(WriterT(ID)(a) = ((a,o₁),o₂)
  (check-equal?
    (with-monad (WriterT AddO (WriterT AddO ID))
      (do
        (tell (cons 1 2))
        (tell (cons 10 10))
        (hijack (tell (cons 3 4)))))
    (cons (cons (cons (void) (cons 3 4)) 11) 12))
  ; WriterT(StateT(ID))(a) = s → ((a,o),s)
  (check-equal?
    (run-StateT 1
      (with-monad (WriterT AddO (StateT #f ID))
        (do
          x ← get
          (tell x)
          (cons _ y) ← (hijack 
                         (do 
                           (tell 10)
                           (tell 11)))
          (put y))))
    (cons (cons (void) 1) 21))
  ; WriterT(FailT(ID))(a) = (a,o)+1
  (check-equal?
    (with-monad (WriterT AddO (FailT ID))
      (hijack 
        (try
          (do
            (tell 1)
            fail)
          (tell 9))))
    (cons (cons (void) 9) 0))
  ; WriterT(ID)(a) = (a,o)+1
  (check-equal?
   (with-monad (WriterT ListO ID)
     (do (cons _ (list 1)) ← (hijack (tell (list 1)))
         (tell (list 9))))
   (cons (void) (list 9)))
  ; WriterT(NondetT(ID))(a) = ℘(a,o)
  (check-equal?
    (with-monad (WriterT AddO (NondetT ID))
      (do
        (mplus (tell 1) (tell 2))
        (cons _ y) ← (hijack (mplus (tell 1) (tell 2)))
        (tell 10)
        (return y)))
    (set (cons 1 11) (cons 1 12) (cons 2 11) (cons 2 12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; StateT s m a ≔ s → m(a,s)
(define (run-StateT s xM) 
  (xM s))
(define (StateT O M)
  (with-monad M
  (monad
    ; return
    (λ (x)
       (λ (s) 
          (return (cons x s))))
    ; bind
    (λ (xM f)
       (λ (s)
          (do
            (cons x s′) ← (xM s)
            ((f x) s′))))
    ; effects
    ((compose
       (if (not (hash-has-key? (monad-effects M) 'reader)) (λ (h) h)
         ; propagating reader effects
         (λ (h) (hash-set h 'reader (monad-reader
                                       ; ask
                                       (λ (s)
                                          (do
                                            r ← ask
                                            (return (cons r s))))
                                       ; local
                                       (λ (r xM)
                                          (λ (s) (local r (xM s))))))))
       (if (not (hash-has-key? (monad-effects M) 'writer)) (λ (h) h)
         ; propagating writer effects
         (λ (h) (hash-set h 'writer (monad-writer
                                       ; tell
                                       (λ (o)
                                          (λ (s)
                                             (do
                                               (tell o)
                                               (return (cons (void) s)))))
                                       ; hijack
                                       (λ (xM)
                                          (λ (s)
                                             (do
                                               (cons (cons x s′) o) ← (hijack (xM s))  ; xM(s) : m(a,s)
                                               (return (cons (cons x o) s′)))))))))
       (if (not (hash-has-key? (monad-effects M) 'state))
         ; the standard state effect
         (λ (h) (hash-set h 'state (monad-state
                                      ; get
                                      (λ (s)
                                         (return (cons s s)))
                                      ; put
                                      (λ (s′)
                                         (λ (s)
                                            (return (cons (void) s′)))))))
         ; combining with an underlying state effect, resulting in a state
         ; effect of pairs: (StateT s₁ ∘ StateT s₂) = StateT (s₁,s₂)
         (λ (h) (hash-set h 'state (monad-state
                                      ; get
                                      (λ (s₁)
                                         (do
                                           s₂ ← get
                                           (return (cons (cons s₁ s₂) s₁))))
                                      ; put
                                      (λ (ss′)
                                         (match-let ([(cons s₁′ s₂′) ss′])
                                           (λ (s₁)
                                              (do
                                                (put s₂′)
                                                (return (cons (void) s₁′))))))))))
       (if (not (hash-has-key? (monad-effects M) 'fail)) (λ (h) h)
         ; propagating fail effects
         (λ (h) (hash-set h 'fail (monad-fail
                                       ; fail
                                       (λ (s) fail)
                                       ; try
                                       (λ (xM₁ xM₂)
                                          (λ (s)
                                            (try (xM₁ s) (xM₂ s))))))))
       (if (not (hash-has-key? (monad-effects M) 'nondet)) (λ (h) h)
         ; propagating nondet effects
         (λ (h) (hash-set h 'nondet (monad-nondet
                                       ; mzero
                                       (λ (s) mzero)
                                       ; mplus
                                       (λ (xM₁ xM₂)
                                          (λ (s)
                                             (mplus (xM₁ s) (xM₂ s)))))))))
     (hash))
    ; properties
    ((compose
       (if (or (not (hash-has-key? (monad-properties M) 'monoid-functor))
               (not (monoid? O))) 
         (λ (h) h)
         ; propagating monoid functor
         (λ (h) (hash-set h 'monoid-functor (λ (O′) 
                                              (with-monoid (monoid-functor (PairO O′ O))
                                                (monoid
                                                  ; ozero
                                                  (λ (s) ozero)
                                                  ; oplus
                                                  (λ (xM₁ xM₂)
                                                     (λ (s)
                                                        (oplus (xM₁ s) (xM₂ s)))))))))))
     (hash)))))

(module+ test
  ; StateT(ID)(a) = s → (a,s)
  (check-equal?
    (with-monad (StateT #f ID)
      (run-StateT 1
        (do
          x ← get
          (put 2)
          y ← get
          (put 3)
          z ← get
          (return (+ x y z)))))
    (cons 6 3))
  ; StateT(ReaderT(ID))(a) = s → r → (a,s)
  (check-equal?
    (run-StateT 1
      (run-ReaderT 2
        (with-monad (StateT #f (ReaderT ID))
          (do
            w ← get
            x ← ask
            y ← (local 3 ask)
            (put (+ y 1))
            z ← get
            (return (+ w x y z))))))
    (cons 10 4))
  ; StateT(WriterT(ID))(a) = s → ((a,s),o)
  (check-equal?
    (run-StateT 1
      (with-monad (StateT #f (WriterT AddO ID))
        (do
          x ← get
          (cons _ y) ← (hijack (tell 2))
          (put y)
          (tell x))))
    (cons (cons (void) 2) 1))
  ; StateT(StateT(ID))(a) = s₁ → s₂ → ((a,s₁),s₂)
  (check-equal?
    (run-StateT 1
      (run-StateT 2
        (with-monad (StateT #f (StateT #f ID))
          (do
            (cons x y) ← get
            (put (cons (* x 2) (* y 2)))))))
    (cons (cons (void) 4) 2))
  ; StateT(FailT(ID))(a) = s → (a,s)+1
  (check-equal?
    (run-StateT 1
      (with-monad (StateT #f (FailT ID))
        (do
          x ← (try
                (do
                  (put 4)
                  fail)
                get)
          (return (+ x 1)))))
    (cons 2 1))
  ; StateT(NondetT(ID))(a) = s → ℘(a,s)
  (check-equal?
    (run-StateT 1
      (with-monad (StateT #f (NondetT ID))
        (do
          (mplus (put 2) (put 3))
          x ← (mplus get mzero)
          (mplus (return (+ x 10)) (return (+ x 20))))))
    (set (cons 12 2) (cons 22 2) (cons 13 3) (cons 23 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fail Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct failure () #:transparent)

; FailT m a ≔ m(a+1)
(define (FailT M)
  (with-monad M
    (monad
      ; return
      return
      ; bind
      (λ (xM f)
         (do
           xO ← xM
           (match xO
             [(failure) (return (failure))]
             [x (f x)])))
      ; effects
      ((compose
         (if (not (hash-has-key? (monad-effects M) 'reader)) (λ (h) h)
           ; propagating reader effects
           (λ (h) (hash-set h 'reader (monad-reader
                                         ; ask
                                         ask
                                         ; local
                                         local))))
         (if (not (hash-has-key? (monad-effects M) 'writer)) (λ (h) h)
           ; propagating writer effects
           (λ (h) (hash-set h 'writer (monad-writer
                                         ; tell
                                         tell
                                         ; hijack
                                         hijack))))
         (if (not (hash-has-key? (monad-effects M) 'state)) (λ (h) h)
           ; propagating state effects
           (λ (h) (hash-set h 'state (monad-state
                                        ; get
                                        get
                                        ; put
                                        put))))
         ; the standard fail effect
         (λ (h) (hash-set h 'fail (monad-fail
                                     ; fail
                                     (return (failure))
                                     ; try
                                     (λ (xM₁ xM₂)
                                        (do
                                          xO ← xM₁
                                          (match xO
                                            [(failure) xM₂]
                                            [x (return x)]))))))
         (if (not (hash-has-key? (monad-effects M) 'nondet)) (λ (h) h)
           (λ (h) (hash-set h 'nondet (monad-nondet
                                         ; mzero
                                         mzero
                                         ; mplus
                                         mplus)))))
       (hash))
      ; properties
      ((compose)
       (hash)))))

(module+ test
  ; FailT(ID)(a) = a+1
  (check-equal?
    (with-monad (FailT ID)
      (do
        (try
          (do
            x ← (return 1)
            fail)
          (do
            x ← (return 1)
            (try
              (return 1)
              fail)
            (return (+ x 1))))))
    2)
  ; FailT(ReaderT(ID))(a) = r → a+1
  (check-equal?
    (run-ReaderT 1
      (with-monad (FailT (ReaderT ID))
        (try
          (do
            x ← ask
            fail)
          (local 2 ask))))
    2)
  ; FailT(WriterT(ID))(a) = (a+1,o)
  (check-equal?
    (with-monad (FailT (WriterT AddO ID))
      (hijack 
        (try
          (do
            (tell 1)
            fail)
          (tell 9))))
    (cons (cons (void) 10) 0))
  ; FailT(StateT(ID))(a) = s → (a+1,s)
  (check-equal?
    (run-StateT 1
      (with-monad (FailT (StateT #f ID))
        (do
          x ← (try
                (do
                  (put 4)
                  fail)
                get)
          (return (+ x 1)))))
    (cons 5 4))
  ; FailT(NondetT(ID)) = ℘(a+1)
  (check-equal?
    (with-monad (FailT (NondetT ID))
      (do
        x ← (try
                (mplus (return 1) fail)
                (return 2))
        y ← (mplus mzero (return 10))
        (return (+ x y))))
    (set 11 12)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Nondet Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NondetT m a ≔ m(℘(a))
(define (NondetT M)
  (with-monad M
    (with-monoid (monoid-functor PowerO)
      (monad
        ; return
        (λ (x)
           (return (set x)))
        ; bind
        (λ (xM f)
           (do
             xs ← xM
             (oconcat (set-map xs f))))
        ; effects
        ((compose
           (if (not (hash-has-key? (monad-effects M) 'reader)) (λ (h) h)
             ; propagating reader effects
             (λ (h) (hash-set h 'reader (monad-reader
                                           ; ask
                                           (do
                                             r ← ask
                                             (return (set r)))
                                           ; local
                                           (λ (r xM)
                                             (local r xM))))))
           (if (not (hash-has-key? (monad-effects M) 'writer)) (λ (h) h)
             ; propagating writer effects
             (λ (h) (hash-set h 'writer (monad-writer
                                           ; tell
                                           (λ (o)
                                              (do
                                                (tell o)
                                                (return (set (void)))))
                                           ; hijack
                                           (λ (xM)
                                              (do
                                                (cons xs o) ← (hijack xM)
                                                (return (set-map xs (λ (x) (cons x o))))))))))
           (if (not (hash-has-key? (monad-effects M) 'state)) (λ (h) h)
             ; propagating state effects
             (λ (h) (hash-set h 'state (monad-state
                                          ; get
                                          (do
                                            s ← get
                                            (return (set s)))
                                          ; put
                                          (λ (s)
                                             (do
                                               (put s)
                                               (return (set (void)))))))))
           (if (not (hash-has-key? (monad-effects M) 'fail)) (λ (h) h)
               ; propagating fail effects
               ; (DD: not sure about this...)
               (λ (h) (hash-set h 'fail (monad-fail
                                         ; fail
                                         fail
                                         ; try
                                         try))))

           ; the standard nondet effect
           (λ (h) (hash-set h 'nondet
                            (monad-nondet
                              ; mzero
                              ozero
                              ; mplus
                              oplus))))
         (hash))
        ((compose
           ; nondet is never a monoid functor, not because it can't be, but
           ; because you should never put a NondetT on top of a NondetT.
           )
         (hash))))))

(module+ test
  ; NondetT(ID)(a) = ℘(a)
  (check-equal?
    (with-monad (NondetT ID)
      (do
        x ← (mplus (return 1) (return 2))
        y ← (mplus (return 3) (return 4))
        (return (cons x y))))
    (set (cons 1 3) (cons 1 4) (cons 2 3) (cons 2 4)))
  ; NondetT(ReaderT(ID))(a) = r → ℘(a)
  (check-equal?
    (run-ReaderT 1
      (with-monad (NondetT (ReaderT ID))
        (do
          x ← (mplus (mplus (local 5 ask) (return 2)) mzero)
          y ← ask
          (return (+ x y)))))
    (set 6 3))
  ; NondetT(WriterT(ID))(a) = (℘(a),o)
  (check-equal?
    (with-monad (NondetT (WriterT MaxO ID))
      (do
        (mplus (tell 1) (tell 2))
        (cons _ y) ← (hijack (mplus (tell 1) (tell 2)))
        (tell 10)
        (return y)))
    (cons (set 2) 10))
  ; NondetT(StateT(ID))(a) = s → (℘(a),s)
  (check-equal?
    (run-StateT 1
      (with-monad (NondetT (StateT MaxO ID))
        (do
          (mplus (put 2) (put 3))
          x ← (mplus get mzero)
          (mplus (return (+ x 10)) (return (+ x 20))))))
    (cons (set 13 23) 3))
  ; NondetT(StateT[FinMap](ID))(a) = [k⇰v] → (℘(a),[k⇰v])
  (check-equal?
    (run-StateT (hash)
      (with-monad (NondetT (StateT (FinMapO MaxO) ID))
        (do
          (mplus (put (∅ 'k 2)) (put (∅ 'k 3)))
          x ← (mplus get mzero)
          (mplus (return (+ (x 'k) 10)) (return (+ (x 'k) 20))))))
    (cons (set 13 23) (∅ 'k 3)))
  ; NondetT(ID)(a) = ℘(a)
  (check-equal?
   (with-monad (NondetT ID)
     (for/monad+ ([x (list 1 2 3 4 5)]) (return x)))
    (set 1 2 3 4 5))
  ; NondetT(ID)(a) = ℘(a)
  (check-equal?
   (with-monad (FailT (StateT #f (NondetT ID)))
     (run-StateT 0 (mplus (for/monad+ ([x (list 1 2 3)]) (return x))
                          fail)))
   (set (cons (failure) 0) (cons 1 0) (cons 2 0) (cons 3 0))))
