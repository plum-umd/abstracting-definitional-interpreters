#lang racket
(require "../signatures.rkt")

(provide δ@ abs-δ@ pres-δ@ symbolic-δ@)

;; Concrete δ
(define-unit δ@
  (import monad^ err^)
  (export δ^)
  (define (δ o . vs)   
    (match* (o vs)
      [('add1 (list n))  (return (add1 n))]
      [('sub1 (list n))  (return (sub1 n))]
      [('- (list n))     (return (- n))]
      [('+ (list n1 n2)) (return (+ n1 n2))]
      [('- (list n1 n2)) (return (- n1 n2))]
      [('* (list n1 n2)) (return (* n1 n2))]
      [('quotient (list n1 n2))
       (if (zero? n2) (err) (quotient n1 n2))]))

  (define (truish? v)
    (return (zero? v))))
  
  

;; Abstract δ
(define-unit abs-δ@
  (import monad^ symbolic^ err^)
  (export δ^)
  (define (δ o . vs)
    (match* (o vs)
      [('add1 (list n))  (return 'N)]
      [('sub1 (list n))  (return 'N)]
      [('+ (list n1 n2)) (return 'N)]
      [('- (list n1 n2)) (return 'N)]
      [('* (list n1 n2)) (return 'N)]
      [('quotient (list n1 (? number? n2)))
       (if (zero? n2)
           (err)
           (return 'N))]
      [('quotient (list n1 n2)) 
       (both (return 'N) (err))]))

  (define (truish? v)
    (match v
      ['N (both (return #t) (return #f))]
      [_  (return (zero? v))])))

;; Precision preserving abstract δ
(define-unit pres-δ@
  (import monad^ symbolic^ err^)
  (export δ^)
  (define (δ o . vs)
    (match* (o vs)
      [('add1 (list (? number? n)))  (return (add1 n))]
      [('sub1 (list (? number? n)))  (return (sub1 n))]
      [('- (list (? number? n)))     (return (- n))]
      [('+ (list (? number? n1) (? number? n2))) (return (+ n1 n2))]
      [('- (list (? number? n1) (? number? n2))) (return (- n1 n2))]
      [('* (list (? number? n1) (? number? n2))) (return (* n1 n2))]
      [('quotient (list (? number? n1) (? number? n2)))
       (if (zero? n2)
           (err)
           (return (quotient n1 n2)))]
      [('add1 (list n))  (return 'N)]
      [('sub1 (list n))  (return 'N)]
      [('+ (list n1 n2)) (return 'N)]
      [('* (list n1 n2)) (return 'N)]
      [('- (list n1 n2)) (return 'N)]
      [('quotient (list n1 (? number? n2)))
       (if (zero? n1)
           (err)
           (return 'N))]
      [('quotient (list n1 n2))
       (both (err) (return 'N))]))

    (define (truish? v)
      (match v
        ['N (both (return #t) (return #f))]
        [_  (return (zero? v))])))

(define-unit symbolic-δ@
  (import monad^ symbolic^ err^)
  (export δ^)
  
  (define (δ o . vs)
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
           (err)
           (return (quotient n1 n2)))]
      [('quotient (list s1 (? number? n2)))
       (if (zero? n2)
           (err)
           (return `(quotient ,s1 ,n2)))]
      [('quotient (list s1 s2))
       (both (err) (return `(quotient ,s1 ,s2)))]))

  (define (truish? v)
    (match v
      [(? number?) (return (zero? v))]
      [_ (both (return #t) (return #f))])))


(define (δ-err o vs)
  (error (format "~a: undefined on ~a" o vs)))

