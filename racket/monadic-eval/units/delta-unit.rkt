#lang racket
(require "../signatures.rkt")

(provide δ@ abs-δ@ pres-δ@ symbolic-δ@)

;; Concrete δ
(define-unit δ@
  (import unit^ err^)
  (export δ^)
  (define (δ o . vs)   
    (match* (o vs)
      [('add1 (list n))  (unit (add1 n))]
      [('sub1 (list n))  (unit (sub1 n))]
      [('- (list n))     (unit (- n))]
      [('+ (list n1 n2)) (unit (+ n1 n2))]
      [('- (list n1 n2)) (unit (- n1 n2))]
      [('* (list n1 n2)) (unit (* n1 n2))]
      [('quotient (list n1 n2))
       (if (zero? n2) (err) (quotient n1 n2))])))
  
  

;; Abstract δ
(define-unit abs-δ@
  (import unit^ symbolic^ err^)
  (export δ^)
  (define (δ o . vs)
    (match* (o vs)
      [('add1 (list n))  (unit 'N)]
      [('sub1 (list n))  (unit 'N)]
      [('+ (list n1 n2)) (unit 'N)]
      [('- (list n1 n2)) (unit 'N)]
      [('* (list n1 n2)) (unit 'N)]
      [('quotient (list n1 (? number? n2)))
       (if (zero? n2)
           (err)
           (unit 'N))]
      [('quotient (list n1 n2)) 
       (both (unit 'N) (err))]))              )





;; Precision preserving abstract δ
(define-unit pres-δ@
  (import unit^ symbolic^ err^)
  (export δ^)
  (define (δ o . vs)
    (match* (o vs)
      [('add1 (list (? number? n)))  (unit (add1 n))]
      [('sub1 (list (? number? n)))  (unit (sub1 n))]
      [('- (list (? number? n)))     (unit (- n))]
      [('+ (list (? number? n1) (? number? n2))) (unit (+ n1 n2))]
      [('- (list (? number? n1) (? number? n2))) (unit (- n1 n2))]
      [('* (list (? number? n1) (? number? n2))) (unit (* n1 n2))]
      [('quotient (list (? number? n1) (? number? n2)))
       (if (zero? n2)
           (err)
           (unit (quotient n1 n2)))]
      [('add1 (list n))  (unit 'N)]
      [('sub1 (list n))  (unit 'N)]
      [('+ (list n1 n2)) (unit 'N)]
      [('* (list n1 n2)) (unit 'N)]
      [('- (list n1 n2)) (unit 'N)]
      [('quotient (list n1 (? number? n2)))
       (if (zero? n1)
           (err)
           (unit 'N))]
      [('quotient (list n1 n2))
       (both (err) (unit 'N))]))
  
  )

(define-unit symbolic-δ@
  (import unit^)
  (export δ^)
  (define (δ o . vs)
    (unit
     (match* (o vs)
       [('add1 (list (? number? n))) (add1 n)]
       [('add1 (list s)) `(add1 ,s)]
       [('+ (list (? number? n) (? number? m))) (+ n m)]
       [('+ (list s t)) `(+ ,s ,t)]       
       [('sub1 (list (? number? n)))  (sub1 n)]
       [('sub1 (list s)) `(sub1 ,s)]       
       [('- (list (? number? n))) (- n)]
       [('- (list s)) `(- ,s)]
       [('- (list (? number? n1) (? number? n2))) (- n1 n2)]
       [('- (list s1 s2)) `(- ,s1 ,s2)]
       [('* (list (? number? n1) (? number? n2))) (* n1 n2)]
       [('* (list s1 s2)) `(* ,s1 ,s2)]       
       [('quotient (list (? number? n1) (? number? n2)))
        (if (zero? n2) 
            'err
            (quotient n1 n2))]
       [('quotient (list s1 (? number? n2)))
        (if (zero? n2)
            'err
            `(quotient ,s1 ,n2))]
       [('quotient (list s1 s2))
        ;; both err and
        `(quotient ,s1 ,s2)]))))


(define (δ-err o vs)
  (error (format "~a: undefined on ~a" o vs)))

