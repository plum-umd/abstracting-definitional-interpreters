#lang racket
(require scribble/eval
	 racket/sandbox)
(provide (all-defined-out))

(define cols 45)

(define (make-monadic-eval link fix)
  (parameterize [(pretty-print-columns cols)]
    (define ev
      (make-base-eval #:pretty-print? #t
                      #:lang 'monadic-eval
                      link
                      fix))
    (set-eval-limits ev 5 200)
    ev))

(define the-pure-eval
   (make-monadic-eval '(monad@ δ@ alloc@ state@ ev@)
                      '(fix ev)))
