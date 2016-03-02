; author: David Darais
; TODO: 
; • lifting effects
; • testing of any kind

#lang racket

(require (for-syntax racket/syntax syntax/parse))

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
  (stream-fold (monoid-oplus O) (monoid-ozero O) (set->stream os)))

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
; mbot : ∀a, M(a)
; mjoin : ∀a, M(a),M(a) → M(a)
(define-struct monad-nondet (mbot mjoin))

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
                   [do (format-id #'e "do")])
       #'(local [(define return (monad-return M))
                 (define bind (monad-bind M))
                 (define-syntax do
                   (syntax-rules (←)
                     [(do xM) xM]
                     [(do p ← xM . bs)
                      (bind xM (λ (x) 
                                  (match-let [(p x)]
                                    (do . bs))))]))]
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

; Reader Monad Transformer
; ReaderT r m a ≔ r → m(a)
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
              (f r x))))
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



; State Monad Transformer
; StateT s m a ≔ s → m(a,s)
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
            (f s′ x))))
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
             (oconcat (map f xs))))
        ; effects
        (hash
          'nondet (make-monad-nondet
                    ; mbot
                    ozero
                    ; mjoin
                    (λ (xM₁ xM₂)
                       (oplus xM₁ xM₂))))))))
