#lang racket/unit
(require racket/match
	 racket/set
	 "../signatures.rkt"
         "../store.rkt"
         "../syntax.rkt"
         #;"../gc.rkt")

;; Bounded store, memoizing abstract set interpreter
;; aka PDCFA


(import)
(export symbolic^ monad^ err^
        return-ans^ return-vals^)

(define ((mrun M) m2)
  (((M (hash)) (hash)) m2))

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

(define ((return-vals vs) s)
  (return-anss (for/set ([v vs])
               (cons v s))))

(define (return-ans v s)
  (return-anss (set (cons v s))))

(define (((return-anss anss) m) m*)
  (cons anss m))

(define ((((return v) s) m) m*)
  (cons (set (cons v s)) m))

(define ((err) s)
  (return-ans 'err s))

;; FO Symbolic values
(define symbolic? symbol?)

(define (symbolic-apply v0 v1)
  (err))

