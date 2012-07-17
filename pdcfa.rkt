#lang racket
(provide eval pdcfa@)
(require "ev-sig.rkt"
         "eval-sig.rkt"
         "ev-monad-sig.rkt"
         "symbolic-monad-sig.rkt"
         "ev-symbolic-unit.rkt"
         "delta-unit.rkt"
         "sto-0cfa-unit.rkt"
         "store.rkt"
         "syntax.rkt")

;; Bounded store, memoizing abstract set interpreter 
;; aka PDCFA

;; eval : E ->_total [Setof Ans]

(define-unit pdcfa@
  (import ev^ δ^ sto-monad^)
  (export eval^ ev-monad^ symbolic-monad^ return^ return-ans^ return-vals^)

  (define (eval e)
    (match (((ev e (hash)) (hash)) (hash))
      [(cons anss m) anss]))
      
  (define (((rec e r) s) m)
    (define ers (list e r s))
    (define anss (hash-ref m ers #false))
    (if anss
        (cons anss m)
        (match (((ev e r) s) (hash-set m ers (set)))
          [(cons anss m)
           (cons anss (hash-set m ers anss))])))
  
  ;; FO Symbolic values
  (define symbolic? symbol?)
    
  (define (symbolic-apply v0 v1)
    (fail))

  (define (((both c0 c1) s) m)
    (match ((c0 s) m)
      [(cons anss0 m)
       (match ((c1 s) m)
         [(cons anss1 m)
          (cons (set-union anss0 anss1) m)])]))
  
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
  
  
  
  (define ((return-vals vs) s)
    (return-anss (for/set ([v vs])
                          (cons v s))))
             
  (define (return-ans v s)
    (return-anss (set (cons v s))))
  
  (define ((return-anss anss) m)
    (cons anss m))
             
  (define (((return v) s) m)
    (cons (set (cons v s)) m))
  
  (define ((fail) s)
    (return-ans 'fail s)))
  

(define-values/invoke-unit/infer  
  (link pdcfa@ ev-symbolic@ abs-delta@ sto-0cfa@))
