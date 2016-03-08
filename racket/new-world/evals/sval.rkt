#lang racket
;; Just a bunch of units thrown in one place. I'll ditch duplicate ones later

(provide eval)
(require "../fix.rkt"
         "../signatures.rkt"
         "../../monad-transformers.rkt"
         "../../monad-transformers-contracted.rkt"
         "../syntax.rkt"
	 )

(define-unit monoid@
  (import)
  (export monoid^)
  (define O PowerO))

(define-unit/contract monad@
  (import)
  (export monad^)
  
  (define M (NondetT (StateT PowerO (ReaderT ID))))
  
  (define (mrun m)
    ;; Path condition represented as set of evaluation known to have evaluated to 0
    (run-StateT (hash) (run-ReaderT (cons (hash) (set)) m))))

(define-unit ev@
  (import monad^)
  (export ev^)

  (define (ev e)
    (with-monad M
      (match e
        [(vbl x)
         (find x)]
        [(num n)
         (return n)]
        [(ifz e0 e1 e2)
         (do
           v ← (ev e0)
           n ← (δ 'negate v)
           (case n
             [(0) (ev e2)]
             [(1) (ev e1)]))]
        [(op1 o e0)
         (do
           v ← (ev e0)
           (δ o v))]
        [(op2 o e0 e1)
         (do
           v0 ← (ev e0)
           v1 ← (ev e1)
           (δ o v0 v1))]
        [(lrc f (lam x e0) e1)
         (rext f (lam x e0) (ev e1))]
        [(lam x e0)
         (do
           ρ ← ask
           (return (cons (lam x e0) ρ)))]
        [(app e0 e1)
         (do
           (cons (lam x e) ρ) ← (ev e0)
           v1                 ← (ev e1)
           (local-env ρ (ext x v1 (ev e))))]))))

(define-unit env@
  (import monad^)
  (export env^)

  (define ask-env (with-monad M ask))
  (define local-env (with-monad M local)))

(define-unit store@
  (import monad^)
  (export store^)
  (define get-store
    (with-monad M
      (do
        (cons σ _) ← get
        (return σ))))
  (define (put-store σ*)
    (with-monad M
      (do
        (cons σ φ) ← get
        (put (cons σ* φ))))))

(define-unit state@
  (import monad^)
  (export state^)

  (define (rext x v m)
    (with-monad M
      (do
        ρ ← ask
        (cons σ φ) ← get
        a ≔ (alloc σ)
        ρ* ≔ (hash-set ρ x a)
        σ* ≔ (hash-set σ a (cons v ρ*))
        (put (cons σ* φ))
        (local-env ρ* m))))
  
  (define (ext x V m)
    (with-monad M
      (do
        ρ ← ask
        (cons σ φ) ← get
        a ≔ alloc σ
        ρ* ≔ (hash-set ρ x a)
        σ* ≔ (hash-set σ a V)
        (put (cons σ* φ))
        (local-env ρ* m))))
  
  (define (find x)
    (with-monad M
      (do
        ρ ← ask
        (cons σ _) ← get
        ;; TODO I'm lost. Does `σ` map to value or set of values?
        ;; The latter would mean I'm wrapping the set again?
        (return (hash-ref σ (hash-ref ρ x)))))))

(define-unit alloc@
  (import)
  (export alloc^)

  (define alloc ; TODO use whatever existing unit
    (let ([n 0])
      (λ _
        (begin0 n (set! n (+ 1 n)))))))

#;(define-unit ref@
  (import monad^)
  (export ref^)
  
  (define (new)
    (error "TODO"))
  
  (define (sbox)
    (error "TODO"))
  
  (define (ubox)
    (error "TODO"))
  )

(define-unit δ@
  (import monad^)
  (export δ^)

  (define (δ o . vs)
    (with-monad M
      (match* (o vs)
        [('add1 (list (? number? n))) (return (add1 n))]
        [('add1 (list s)) (return `(add1 ,s))]
        [('+ (list (? number? n) (? number? m))) (return (+ n m))]
        [('+ (list s t)) (return `(+ ,s ,t))]       
        [('sub1 (list (? number? n)))  (return (sub1 n))]
        [('sub1 (list s)) (return `(sub1 ,s))]       
        [('- (list (? number? n))) (return (- n))]
        [('- (list s)) (return `(- ,s))]
        [('- (list (? number? n1) (? number? n2))) (return (- n1 n2))]
        [('- (list s1 s2)) (return `(- ,s1 ,s2))]
        [('* (list (? number? n1) (? number? n2))) (return (* n1 n2))]
        [('* (list s1 s2)) (return `(* ,s1 ,s2))]       
        [('quotient (list (? number? n1) (? number? n2)))
         (if (zero? n2)
             (fail)
             (return (quotient n1 n2)))]
        [('quotient (list s1 (? number? n2)))
         (if (zero? n2)
             (fail)
             (return `(quotient ,s1 ,n2)))]
        [('quotient (list s1 s2))
         (do
           (cons σ φ) ← get
           (case (proves-0 φ (op1 'negate s2))
             [(✓) (return `(quotient ,s1 ,s2))]
             [(✗) (fail)]
             [(?) (mplus ; TODO: or the other way around?
                   (do
                     (refine (op1 'negate s2))
                     (fail))
                   (do
                     (refine s2) 
                     (return `(quotient ,s1 ,s2))))]))]
        [('negate (list v)) ; return 1 for 0; 0 for anything else
         (do
           (cons _ φ) ← get
           (case (proves-0 φ v)
             [(✓) (return 1)]
             [(✗) (return 0)]
             [(?) (mplus ; TODO: or the other way around???
                   (do
                     (refine (op1 'negate v))
                     (return 0))
                   (do
                     (refine (op1 'negate v))
                     (return 1)))]))])))

  (define (truish? v)
    (with-monad M
      (match v
        [(? number?) (return (zero? v))]
        [_ (mplus (return #t) (return #f))])))

    ;; The proof relation is internal to `δ` for now
  (define (proves-0 φ e) ; TODO more precise
    (match e
      [0 '✓]
      [_
       (cond
         [(set-member? φ e) '✓]
         [else '?])]))

  (define (refine e)
    (with-monad M
      (do
        (cons σ φ) ← get
        (put (cons σ (set-add φ e)))))))

(define-unit symbolic@
  (import monad^)
  (export symbolic^)
  
  (define (symbolic? x)
    (or (symbol? x) (cons? x))))

(define-values/invoke-unit/infer
  (link monoid@ monad@ ev@ env@ store@ state@ alloc@ δ@ symbolic@))

(define (eval e)
  (mrun ((fix ev) e)))
