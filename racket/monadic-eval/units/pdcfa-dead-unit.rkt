#lang racket/unit
(require racket/match
	 racket/set
	 "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt"
	 "../subexp.rkt")

;; Bounded store, memoizing abstract set interpreter
;; aka PDCFA
;; with dead code computation

;; eval : E ->_total [Setof Ans] * [Setof Exp]

;; Computes possible results and the set of expressions that are never
;; touched.

;; NB: you could also compute [Setof (Ans * Exp)] which would compute
;; context sensitive dead-code.

(import ev^)
(export eval^ symbolic^ rec^ unit^ unit-ans^ unit-vals^ bind^ err^)

;; iterates ev until reaching a fixed point in the memo-table
(define (eval e)
  (let loop ([m* (hash)] [anss (set)] [dead (subexps e)])
    (match (((((rec e (hash)) (hash)) dead) (hash)) m*)
      [(cons (cons anss1 d) m1)
       (if (and (equal? anss1 anss) (equal? m1 m*))
	   (cons anss1 d)
	   (loop m1 anss1 d))])))

(define (((((rec e r) s) d) m) m*)
  (define ers (list e r s))
  (define anss (hash-ref m ers #false))
  
  ;; like ev but takes both branches on abstract values
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
  
  (if anss
      (cons (cons anss d) m)
      (match (((((ev* e r) s) (set-remove d e))
	       (hash-set m ers (hash-ref m* ers (set)))) m*)
        [(cons (cons anss d) m)
         (cons (cons anss d) (hash-set m ers anss))])))

;; FO Symbolic values
(define symbolic? symbol?)

(define (symbolic-apply v0 v1)
  (err))

(define (((((both c0 c1) s) d) m) m*)
  (match ((((c0 s) d) m) m*)
    [(cons (cons anss0 d) m)
     (match ((((c1 s) d) m) m*)
       [(cons (cons anss1 d) m)
        (cons (cons (set-union anss0 anss1) d) m)])]))

(define (((((bind a f) s) d) m) m*)
  (match ((((a s) d) m) m*)
    [(cons (cons anss d) m)
     (let-values
         ([(anss m d)
           (for*/fold ([rs (set)]
                       [m m]
		       [d d])
             ([ans anss])
             (match ans
               [(cons 'err s)
                (values (set-union rs (set (cons 'err s))) m d)]
               [(cons v s)
                (match (((((f v) s) d) m) m*)
                  [(cons (cons anss d) m) (values (set-union rs anss) m d)])]))])
       (cons (cons anss d) m))]))

(define ((unit-vals vs) s)
  (unit-anss (for/set ([v vs])
               (cons v s))))

(define (unit-ans v s)
  (unit-anss (set (cons v s))))

(define ((((unit-anss anss) d) m) m*)
  (cons (cons anss d) m))

(define (((((unit v) s) d) m) m*)
  (cons (cons (set (cons v s)) d) m))

(define ((err) s)
  (unit-ans 'err s))


(define-syntax do
    (syntax-rules (←)
      [(do b) b]
      [(do x ← e . r)
       (bind e (λ (x) (do . r)))]))