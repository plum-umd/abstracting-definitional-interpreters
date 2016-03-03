#lang racket/unit
(require racket/match
         racket/set
         "../signatures.rkt"
         "../store.rkt"
	 "../both.rkt")

(import)
(export monad^ symbolic^ ref^)

;; State + Non-deterministic monad

(define (mrun M) (M (hash)))

(define (symbolic? x) (or (symbol? x) (pair? x)))

;; Given 2 actions, return new action mapping a state `s` to possibilities
;; by performing each given action
(define ((both c0 c1) s)
  {set-union (c0 s) (c1 s)})

(define (symbolic-apply f v)
  (return `(,f ,v)))

(define ((return v) s)
  {set (cons v s)})

(define ((bind a f) s)
  (for/fold ([acc {set}]) ([res (in-set (a s))])
    (match-define (cons a s*) res)
    (match a
      ['err (set-add acc res)]
      [v (set-union acc ((f v) s*))])))

(define ((new v) s)
  (define a (next s))
  ((return a) (update-sto s a v)))

(define ((sbox a v) s)
  ((return a) (update-sto s a v)))

(define ((ubox a) s)
  ((return (lookup-sto s a)) s))
