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

(define-syntax (with-monoid stx)
  (syntax-parse stx
    [(with-monoid O e)
     (with-syntax ([ozero (format-id #'e "ozero")]
                   [oplus (format-id #'e "oplus")]
                   [oconcat (format-id #'e "oconcat")])
       #'(local [(define ozero (monoid-ozero O))
                 (define oplus (monoid-oplus O))
                 (define oconcat (monoid-oconcat O))]
           e))]))

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
       #'(local [(define return (monad-return M))
                 (define bind (monad-bind M))
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
                 (define ask       (monad-reader-ask       (dict-ref (monad-effects M) 'reader (make-monad-reader false false))))
                 (define local-env (monad-reader-local-env (dict-ref (monad-effects M) 'reader (make-monad-reader false false))))
                 (define tell      (monad-writer-tell      (dict-ref (monad-effects M) 'writer (make-monad-writer false false))))
                 (define hijack    (monad-writer-hijack    (dict-ref (monad-effects M) 'writer (make-monad-writer false false))))
                 (define get       (monad-state-get        (dict-ref (monad-effects M) 'state  (make-monad-state false false))))
                 (define put       (monad-state-put        (dict-ref (monad-effects M) 'state  (make-monad-state false false))))
                 (define mzero     (monad-nondet-mzero     (dict-ref (monad-effects M) 'nondet (make-monad-nondet false false))))
                 (define mplus     (monad-nondet-mplus     (dict-ref (monad-effects M) 'nondet (make-monad-nondet false false))))]
           e))]))

;;;;;;;;;;;;;
; Instances ;
;;;;;;;;;;;;;

; Identity monad
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

; Reader Monad Transformer
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
      (hash
        'reader (make-monad-reader
                         ; ask
                         (λ (r)
                            (return r))
                         ; local-env
                         (λ (r′ xM)
                            (λ (r)
                               (xM r′))))))))

(module+ test
  (check-equal?
    (run-ReaderT 1
      (with-monad (ReaderT ID)
        (do
          x ← ask
          y ← (local-env 10 ask)
          (return (+ x y)))))
    11))

(define AddO
  (make-monoid 0 +))

; Writer Monad Transformer
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
    (hash
      'writer (make-monad-writer
                ; tell
                (λ (o)
                   (return (cons (void) o)))
                ; hijack
                (λ (xM)
                   (do
                     (cons x o) ← xM
                     (return (cons (cons x o) ozero))))))))))

(module+ test
  (check-equal?
    (with-monad (WriterT AddO ID)
      (do
        (cons _ n) ← (hijack
                       (do
                         (tell 1)
                         (tell 2)))
        (tell 10)
        (return n)))
    (cons 3 10)))

; State Monad Transformer
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

; Powerset Commutative Monoid

(define PowerO
  (make-monoid
    ; ozero
    (set)
    ; oplus
    set-union))

; Nondet Monad Transformer
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
