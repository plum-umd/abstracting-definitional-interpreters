#lang racket/unit
(require racket/match
	 racket/set
	 "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt"
         #;"../gc.rkt")

;; Bounded store, memoizing abstract set interpreter
;; aka PDCFA

;; eval : E ->_total [Setof Ans]

(import ev^)
(export eval^ symbolic^ unit^ bind^ err^
        unit-ans^ unit-vals^)

;; iterates ev until reaching a fixed point in the memo-table
(define (eval e)
  
  (define ((((ev′ e ρ) σ) m1) m2)
    (define ers (list e ρ σ))
    (define anss (hash-ref m1 ers #false))  
    (if anss
        (cons anss m1)
        (match (((((ev e ρ) ev′) σ)
                 (hash-set m1 ers (hash-ref m2 ers (set)))) m2)                
          [(cons anss m1)
           (cons anss (hash-set m1 ers anss))])))
  
  
  (let loop ([m2 (hash)] [anss (set)])
    (match ((((ev′ e (hash)) (hash)) (hash)) m2)
      [(and r (cons anss1 m1))
       (if (equal? r (cons anss m2))
	   anss1
	   (loop m1 anss1))])))



;; like ev but takes both branches on abstract values
#;
(define (ev* e r)
  (match e
    [(ifz e0 e1 e2)
     (do v ← (rec e0 r)
       (match v
         [0           (rec e1 r)]
         [(? number?) (rec e2 r)]
         [(? symbolic?)
          (both (rec e1 r)
                (rec e2 r))]))]
    [_  (ev e r)]))






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
               [(cons 'err s)
                (values (set-union rs (set (cons 'err s))) m)]
               [(cons v s)
                (match ((((f v) s) m) m*)
                  [(cons anss m) 
                   (values (set-union rs anss) m)])]))])
       (cons anss m))]))




(define-syntax do
    (syntax-rules (←)
      [(do b) b]
      [(do x ← e . r)
       (bind e (λ (x) (do . r)))]))




(define ((unit-vals vs) s)
  (unit-anss (for/set ([v vs])
               (cons v s))))

(define (unit-ans v s)
  (unit-anss (set (cons v s))))

(define (((unit-anss anss) m) m*)
  (cons anss m))

(define ((((unit v) s) m) m*)
  (cons (set (cons v s)) m))

(define ((err) s)
  (unit-ans 'err s))

;; FO Symbolic values
(define symbolic? symbol?)

(define (symbolic-apply v0 v1)
  (err))

;; ev just once
#;
(define (eval e)
  (match (((ev e (hash)) (hash)) (hash))
    [(cons anss m) anss]))
