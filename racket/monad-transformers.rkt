; author: David Darais
; TODO: 
; • lifting effects
; • testing of any kind

#lang racket

(require (for-syntax racket/syntax syntax/parse))
(require rackunit)

;;;;;;;;;;
; Monoid ;
;;;;;;;;;;

; A monoid is a collection of ozero+oplus operations. ozero is a unit and oplus
; is associative and sometimes commutative.
; monoid (O : type) : type
; ozero : O
; oplus : O,O → O
(define-struct monoid (ozero oplus))

(define ((monoid-oconcat O) os)
  (sequence-fold (monoid-oplus O) (monoid-ozero O) os))

;;;;;;;;;
; Monad ;
;;;;;;;;;

; A monad is a collection of return+bind operations and a map from supported
; effect names to their implementations. Monad transformers will propagate as
; many effect operations as they know how.

; monad (M : type → type) : type
; return : ∀a, a → M(a)
; bind : ∀ab, M(a),(a → M(b)) → M(b)
; effects : effect-name ⇰ effect-for(effect-name)
; effect-name ∈ {reader, writer, state, nondet}
; effect-for : effect-name → type
(define-struct monad (return bind effects))

;;;;;;;;;;;
; Effects ;
;;;;;;;;;;;

; Effects are operations supported by some monad

; effect-for(reader) = monad-reader
; monad-reader (r : type) (M : type → type) : type
; ask : M(r)
; local-env : ∀a, r,M(a) → M(a)
(define-struct monad-reader (ask local-env))

; effect-for(writer) = monad-writer
; monad-writer (o : type) (M : type → type) : type
; tell : o → M(unit)
; hijack : ∀a, M(a) → M(a,o)
(define-struct monad-writer (tell hijack))

; effect-for(state) = monad-state
; monad-state (s : type) (M : type → type) : type
; get : M(s)
; put : s → M(unit)
(define-struct monad-state (get put))

; effect-for(nondet) = monad-nondet
; nondet (M : type → type) : type
; mzero : ∀a, M(a)
; mplus : ∀a, M(a),M(a) → M(a)
(define-struct monad-nondet (mzero mplus))

;;;;;;;;;;
; Syntax ;
;;;;;;;;;;

; `(with-monoid O e)` introduces `ozero`, `oplus` and `oconcat` specialized to
; the monoid `O` into scope inside of `e`.
(define-syntax (with-monoid stx)
  (syntax-parse stx
    [(with-monoid O e)
     (with-syntax ([ozero (format-id #'e "ozero")]
                   [oplus (format-id #'e "oplus")]
                   [oconcat (format-id #'e "oconcat")])
       #'(local [(define O′ O)
                 (define ozero (monoid-ozero O′))
                 (define oplus (monoid-oplus O′))
                 (define oconcat (monoid-oconcat O′))]
           e))]))
; `(with-monad M e)` introduces monad operations and do notation into scope
; inside of `e`. There might be a better way to do this (or at least automate
; it) but I have bigger fish to fry at the moment.
(define-syntax (with-monad stx)
  (syntax-parse stx
    [(with-monad M e)
     (with-syntax ([return (format-id #'e "return")]
                   [bind (format-id #'e "bind")]
                   [do (format-id #'e "do")]
                   [ask (format-id #'e "ask")]
                   [local-env (format-id #'e "local-env")]
                   [tell (format-id #'e "tell")]
                   [hijack (format-id #'e "hijack")]
                   [get (format-id #'e "get")]
                   [put (format-id #'e "put")]
                   [mzero (format-id #'e "mzero")]
                   [mplus (format-id #'e "mplus")])
       #'(local [(define M′ M)
                 (define return (monad-return M′))
                 (define bind (monad-bind M′))
                 (define-syntax do
                   (syntax-rules (←)
                     [(do xM) xM]
                     [(do p ← xM . bs)
                      (bind xM (λ (x) 
                                  (match-let [(p x)]
                                    (do . bs))))]
                     [(do xM . bs)
                      (bind xM (λ (x)
                                  (do . bs)))]))
                 (define ask       (monad-reader-ask       (dict-ref (monad-effects M′) 'reader (make-monad-reader false false))))
                 (define local-env (monad-reader-local-env (dict-ref (monad-effects M′) 'reader (make-monad-reader false false))))
                 (define tell      (monad-writer-tell      (dict-ref (monad-effects M′) 'writer (make-monad-writer false false))))
                 (define hijack    (monad-writer-hijack    (dict-ref (monad-effects M′) 'writer (make-monad-writer false false))))
                 (define get       (monad-state-get        (dict-ref (monad-effects M′) 'state  (make-monad-state false false))))
                 (define put       (monad-state-put        (dict-ref (monad-effects M′) 'state  (make-monad-state false false))))
                 (define mzero     (monad-nondet-mzero     (dict-ref (monad-effects M′) 'nondet (make-monad-nondet false false))))
                 (define mplus     (monad-nondet-mplus     (dict-ref (monad-effects M′) 'nondet (make-monad-nondet false false))))]
           e))]))

;;;;;;;;;;;;;
; Instances ;
;;;;;;;;;;;;;

; Powerset Commutative Monoid
(define PowerO
  (make-monoid
    ; ozero
    (set)
    ; oplus
    set-union))

; Addition Commutative Monoid
(define AddO
  (make-monoid 
    ; ozero
    0 
    ; oplus
    +))

;;;;;;;;;;;;;;;;;;
; Identity monad ;
;;;;;;;;;;;;;;;;;;

; ID a ≔ a
(define ID (make-monad
             (λ (x) x)
             (λ (xM f) (f xM))
             (hash)))
(define (IDJF JF) JF)

(module+ test
  (check-equal?
    (with-monad ID
      (do
        x ← (return 1)
        y ← (return (+ x 1))
        (return (+ y 1))))
    3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reader Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ReaderT r m a ≔ r → m(a)
(define (run-ReaderT r xM)
  (xM r))
(define (ReaderT M)
  (with-monad M
    (make-monad
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
           (λ (h) (hash-set h 'reader (make-monad-reader
                                         ; ask
                                         (λ (r)
                                            (return r))
                                         ; local-env
                                         (λ (r′ xM)
                                            (λ (r)
                                               (xM r′))))))
           ; combining with an underlying reader effect, resulting in a reader
           ; effect of pairs: (ReaderT r₁ ∘ ReaderT r₂) = ReaderT (r₁,r₂)
           (λ (h) (hash-set h 'reader (make-monad-reader
                                         ; ask
                                         (λ (r)
                                            (do
                                              rₘ ← ask
                                              (return (cons r rₘ))))
                                         (λ (rrₘ′ xM)
                                            (λ (r)
                                               (match-let ([(cons r′ rₘ′) rrₘ′])
                                                 (local-env rₘ′ (xM r′)))))))))
         (if (not (hash-has-key? (monad-effects M) 'writer)) (λ (h) h)
           ; propagating writer effects
           (λ (h) (hash-set h 'writer (make-monad-writer
                                         ; tell
                                         (λ (o)
                                           (λ (r)
                                             (tell o)))
                                         ; hijack
                                         (λ (xM)
                                            (λ (r) (hijack (xM r))))))))
         (if (not (hash-has-key? (monad-effects M) 'state)) (λ (h) h)
           ; propagating state effects
           (λ (h) (hash-set h 'state (make-monad-state
                                        ; get
                                        (λ (r)
                                           get)
                                        ; put
                                        (λ (s)
                                           (λ (r)
                                              (put s)))))))
         (if (not (hash-has-key? (monad-effects M) 'nondet)) (λ (h) h)
           ; propagating nondet effects
           (λ (h) (hash-set h 'nondet (make-monad-nondet
                                         ; mzero
                                         (λ (r) mzero)
                                         ; mplus
                                         (λ (xM₁ xM₂)
                                            (λ (r) (mplus (xM₁ r) (xM₂ r)))))))))
       (hash)))))

(module+ test
  ; ReaderT(ID)(a) = r → a
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT ID)
        (do
          x ← ask
          y ← (local-env 10 ask)
          (return (+ x y)))))
    11)
  ; ReaderT(ReaderT(ID))(a) = r₁ → r₂ → a
  (check-equal?
    (run-ReaderT 1
      (run-ReaderT 2
        (with-monad (ReaderT (ReaderT ID))
          (do
            (cons x₁ y₁) ← ask
            (cons x₂ y₂) ← (local-env (cons 3 4) ask)
            (return (+ (* x₁ x₁) y₁ x₂ y₂))))))
    12)
  ; ReaderT(WriterT(ID))(a) = r → (a,o)
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT (WriterT AddO ID))
        (do
          x ← ask
          (tell (+ x 1))
          (tell (+ x 2))
          (return (+ x 3)))))
    (cons 4 5))
  ; ReaderT(StateT(ID))(a) = r → s → (a,s)
  (check-equal?
    (run-StateT 10
      (run-ReaderT 1
        (with-monad (ReaderT (StateT ID))
          (do
            x ← ask
            y ← get
            (put 100)
            (return (+ x y))))))
    (cons 11 100))
  ; ReaderT(NondetT(ID))(a) = r → ℘(a)
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT (NondetT IDJF ID))
        (do
          x ← (mplus ask (return 2))
          y ← ask
          (return (+ x y)))))
    (set 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Writer Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; WriterT o m a ≔ m(a,o)
(define (WriterT O M)
  (with-monad M
    (with-monoid O
      (make-monad
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
             (λ (h) (hash-set h 'reader (make-monad-reader
                                           ; ask
                                           (do
                                             r ← ask
                                             (return (cons r ozero)))
                                           ; local-env
                                           (λ (r xM)
                                              (local-env r xM))))))
           (if (not (hash-has-key? (monad-effects M) 'writer))
             ; the standard writer effect
             (λ (h) (hash-set h 'writer (make-monad-writer
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
             (λ (h) (hash-set h 'writer (make-monad-writer
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
             (λ (h) (hash-set h 'state (make-monad-state
                                          ; get
                                          (do
                                            s ← get
                                            (return (cons s ozero)))
                                          ; put
                                          (λ (s)
                                             (do
                                               put s
                                               (return (cons void ozero))))))))
           (if (not (hash-has-key? (monad-effects M) 'nondet)) (λ (h) h)
             ; propagating nondet effects
             (λ (h) (hash-set h 'nondet (make-monad-nondet
                                           ; mzero
                                           mzero
                                           ; mplus
                                           (λ (xM₁ xM₂) (mplus xM₁ xM₂)))))))
         (hash))))))

(module+ test
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
    (cons 6 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; StateT s m a ≔ s → m(a,s)
(define (run-StateT s xM) (xM s))
(define (StateT M)
  (with-monad M
  (make-monad
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
    (hash
      'state (make-monad-state
               ; get
               (λ (s)
                  (return (cons s s)))
               ; put
               (λ (s′)
                  (λ (s)
                     (return (cons (void) s′)))))))))

(module+ test
  (check-equal?
    (with-monad (StateT ID)
      (run-StateT 1
        (do
          x ← get
          (put 2)
          y ← get
          (put 3)
          z ← get
          (return (+ x y z)))))
    (cons 6 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Nondet Monad Transformer ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NondetT m a ≔ m(℘(a))
(define (NondetT JF M)
  (with-monad M
    (with-monoid (JF PowerO)
      (make-monad
        ; return
        (λ (x)
           (return (set x)))
        ; bind
        (λ (xM f)
           (do
             xs ← xM
             (oconcat (set-map xs f))))
        ; effects
        (hash
          'nondet (make-monad-nondet
                    ; mzero
                    ozero
                    ; mplus
                    (λ (xM₁ xM₂)
                       (oplus xM₁ xM₂))))))))

(module+ test
  (check-equal?
    (with-monad (NondetT IDJF ID)
      (do
        x ← (mplus (return 1) (return 2))
        y ← (mplus (return 3) (return 4))
        (return (cons x y))))
    (set (cons 1 3) (cons 1 4) (cons 2 3) (cons 2 4))))
