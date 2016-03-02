#lang racket/unit
(require racket/match
	 racket/set
         "../signatures.rkt")

(import ev^)
(export return^ return-ans^ return-vals^ bind^ run^)

(define (mrun M) (M (hash)))

(define ((return v) s) (set (cons v s)))

(define ((bind a f) s)
  (for*/fold ([rs (set)])
    ([ret (a s)])
    (set-union rs
	       (match ret
                 [(cons 'err s) (set (cons 'err s))]
		 [(cons v s)
		  ((f v) s)]))))

(define ((return-vals vs) s)
  (return-anss (for/set ([v vs])
               (cons v s))))

(define (return-ans v s)
  (return-anss (set (cons v s))))

(define (return-anss anss) anss)
