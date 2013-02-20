#lang racket
(provide eval pdcfa@)
(require "signatures.rkt"
         "ev-symbolic-unit.rkt"
         "delta-unit.rkt"
         "sto-0cfa-unit.rkt"
         "store.rkt"
         "syntax.rkt")

;; Bounded store, memoizing abstract set interpreter
;; aka PDCFA

;; eval : E ->_total [Setof Ans]

(define-unit pdcfa@
  (import ev^)
  (export eval^ symbolic-monad^ unit^ unit-ans^ unit-vals^ bind^ fail^)

  ;; iterates ev until reaching a fixed point in the memo-table
  (define (eval e)
    (let loop ([m* (hash)] [anss (set)])
      (match ((((ev e (hash)) (hash)) (hash)) m*)
        [(and r (cons anss1 m1))
	 (if (equal? r (cons anss m*))
             anss1
             (loop m1 anss1))])))

  ;; ev just once
  #;
  (define (eval e)
    (match (((ev e (hash)) (hash)) (hash))
      [(cons anss m) anss]))

  (define ((((rec e r) s) m) m*)
    (define ers (list e r s))
    (define anss (hash-ref m ers #false))
    (if anss
        (cons anss m)
        (match ((((ev e r) s) (hash-set m ers (hash-ref m* ers (set)))) m*)
          [(cons anss m)
           (cons anss (hash-set m ers anss))])))

  ;; FO Symbolic values
  (define symbolic? symbol?)

  (define (symbolic-apply v0 v1)
    (fail))

  (define ((((both c0 c1) s) m) m*)
    (match (((c0 s) m) m*)
      [(cons anss0 m)
       (match (((c1 s) m) m*)
         [(cons anss1 m)
          (cons (set-union anss0 anss1) m)])]))

  (define ((((bind a f) s) m) m*)
    (match (((a s) m) m*)
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
                  (match ((((f v) s) m) m*)
                    [(cons anss m) (values (set-union rs anss) m)])]))])
         (cons anss m))]))

  (define ((unit-vals vs) s)
    (unit-anss (for/set ([v vs])
                          (cons v s))))

  (define (unit-ans v s)
    (unit-anss (set (cons v s))))

  (define (((unit-anss anss) m) m*)
    (cons anss m))

  (define ((((unit v) s) m) m*)
    (cons (set (cons v s)) m))

  (define ((fail) s)
    (unit-ans 'fail s)))


(define-values/invoke-unit/infer
  (link pdcfa@ ev-symbolic@ abs-δ@ sto-0cfa@))
