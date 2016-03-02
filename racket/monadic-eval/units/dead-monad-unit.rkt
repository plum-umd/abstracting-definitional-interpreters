#lang racket/unit
(require racket/match
         "../signatures.rkt"
         "../subexp.rkt")
(import)
(export return^ bind^ #;run^)

;; FIXME: can't write mrun here because need expr.
#;
(define (mrun M) ((M (hash)) (subexps e)))

;; Same as trace monad
(define (((return v) s) t) (cons (cons v s) t))
(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t) (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
