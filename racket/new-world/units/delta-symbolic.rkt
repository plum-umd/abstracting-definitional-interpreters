#lang racket/unit

(require racket/match racket/set
         "../syntax.rkt" "../signatures.rkt" "../../monad-transformers.rkt")

(import monad^ symbolic^)
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
       (do φ ← get-path-cond
           (case (proves-0 φ (op1 'flip s2))
             [(✓) (fail)]
             [(✗) (return `(quotient ,s1 ,s2))]
             [(?) (mplus ; TODO: or the other way around?
                   (do (refine s2)
                       (fail))
                   (do (refine (op1 'flip s2)) 
                       (return `(quotient ,s1 ,s2))))]))]
      [('flip (list v)) ; return 1 for 0; 0 for anything else
       (do φ ← get-path-cond
           (case (proves-0 φ v)
             [(✓) (return 1)]
             [(✗) (return 0)]
             [(?) (mplus ; TODO: or the other way around???
                   (do (refine (op1 'flip v))
                       (return 0))
                   (do (refine v)
                       (return 1)))]))])))

(define (truish? v) ; defer to `δ` for more precision when proof relation works
  (with-monad M
    (do ~v ← (op1 'flip v)
      (return
       (case ~v
         [(0) #f]
         [(1) #t])))))

;; The proof relation is internal to `δ` for now
(define (proves-0 φ e) ; TODO more precise
  (match e
    [0 '✓]
    [(? number?) '✗]
    [(cons (lam _ _) _) '✗]
    [e #:when (set-member? φ e) '✓]
    [e #:when (set-member? φ (op1 'add1 e)) '✗]
    [(op1 'add1 e*) (flip-R (proves-0 φ e*))]
    [_ '?]))

(define (flip-R R)
  (case R
    [(✓) '✗]
    [(✗) '✓]
    [(?) '?]))
