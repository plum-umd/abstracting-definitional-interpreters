#lang racket/unit
(require racket/match
	 "../signatures.rkt")

(import rec^)
(export eval^)

(define (eval e) ((rec e (hash)) (hash)))
