#lang racket

(require racket/match racket/set
         "../syntax.rkt" "../signatures.rkt" "../transformers.rkt")
(provide δ-symbolic@)

(define-unit δ-symbolic@
  (import monad^ msymbolic^)
  (export δ^)

  (define (δ o . vs)
    (with-monad M
      (match* (o vs)
        ; `N` is unrefinable, cannot build up symbolic value from it
        [((not '/) (list _ ... 'N _ ...)) (return 'N)]
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
        [('/ (list (? number? n1) (? number? n2)))
         (if (zero? n2)
             fail
             (return (quotient n1 n2)))]
        [('/ (list s1 (? number? n2)))
         (if (zero? n2)
             fail
             (return `(/ ,s1 ,n2)))]
        [('/ (list _ 'N))
         (mplus (return 'N) fail)]
        [('/ (list s1 s2))
         (do b ← (truish? s2) ; relies on `s2`'s range being just numbers
           (if b fail (return `(/ ,s1 ,s2))))]
        [('¬ (list v))
         (do b ← (truish? v)
           (return (if b 1 0)))])))

  (define (truish? v)
    (with-monad M
      (case v
        [(N) (mplus (return #t) (return #f))]
        [else
           (do φ ← get-path-cond
             (case (proves-0 φ v)
               [(✓) (return #t)]
               [(✗) (return #f)]
               [(?) (mplus
                     (do (refine v)
                         (return #t))
                     (do (refine `(¬ ,v))
                         (return #f)))]))]))))

;; The proof relation is internal to `δ` for now (not part of any public interface)
(define (proves-0 φ e) ; TODO more precise
  (match e
    [0 '✓]
    [(? number?) '✗]
    [(cons (lam _ _) _) '✗]
    [e #:when (set-member? φ e) '✓]
    [e #:when (set-member? φ `(¬ ,e)) '✗]
    [`(¬ ,e*) (¬-R (proves-0 φ e*))]
    [_ '?]))

(define (¬-R R)
  (case R
    [(✓) '✗]
    [(✗) '✓]
    [(?) '?]))
