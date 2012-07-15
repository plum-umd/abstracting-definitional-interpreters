#lang racket
(provide eval pdcfa@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "symbolic-monad-sig.rkt"
         "ev-symbolic-unit.rkt"
         "store.rkt"
         "syntax.rkt")

;; Bounded store, memoizing abstract set interpreter 
;; aka PDCFA

;; eval : E ->_total [Setof Ans]

(define-unit pdcfa@
  (import ev^)
  (export eval^ ev-monad^ symbolic-monad^)
  
  (define (symbolic-apply v0 v1)
    (fail))
  
  (define symbolic? symbol?)
  
  (define (((both v0 v1) s) m)
    (cons (set (cons v0 s) (cons v1 s)) m))

  
  (define (eval e)
    (match (((ev e (hash)) (hash)) (hash))
      [(cons anss m) anss]))
      
  (define (((rec e r) s) m)
    (define ers (list e r s))
    (define anss (hash-ref m ers #false))
    (if anss
        (cons anss m)
        (match (((ev e r) s) (hash-set m ers (set)))
          [(cons anss m) (cons anss (hash-set m ers anss))])))
  
  (define (((return v) s) m) (cons (set (cons v s)) m))
  (define (((fail) s) m) (cons (set (cons 'fail s)) m))
    
  (define (((bind a f) s) m)
    (match ((a s) m)
      [(cons anss m)
       (let-values 
           ([(anss m)
             (for*/fold ([rs (set)]
                         [m m])
               ([ans anss])
               (match ans
                 [(cons 'fail s)
                  (values (set-union rs (set (cons 'fail s))) m)]
                 [(cons v s)
                  (match (((f v) s) m)
                    [(cons anss m) (values (set-union rs anss) m)])]))])
         (cons anss m))]))
  
  (define (((lookup-env r x) s) m)
    (cons (for/set [(v (lookup s r x))]
                   (cons v s))
          m))
  
  (define (((alloc f v) s) m)
    (cons (match f
            [(cons (lam x e) r)
             (define a x) ; 0CFA-like abstraction
             (set (cons a (join-sto s a v)))])
          m))
  
  (define (((new v) s) m)    
    (define a 'box) ; One box per program abstraction
    (cons (set (cons a (join-sto s a v)))
          m))
  
  (define (((sbox a v) s) m)
    (cons (set (cons a (join-sto s a v))) m))
  
  (define (((ubox a) s) m)
    (cons (for/set ((v (lookup-sto s a)))
                   (cons v s))
          m))
  
  (define (((δ o . vs) s) m)
    (cons (set 
           (match* (o vs)
             [('add1 (list n))  (cons 'N s)]
             [('+ (list n1 n2)) (cons 'N s)]))
          m)))
  

(define-values/invoke-unit/infer  
  (link pdcfa@ ev-symbolic@))
