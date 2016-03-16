#lang racket
(require scribble/eval
	 racket/sandbox)
(provide (all-defined-out))

(define cols 45)

(define (make-monadic-eval link fix)
  (parameterize [(pretty-print-columns cols)]
    (define ev
      (make-base-eval #:pretty-print? #t
                      #:lang 'monadic-eval/lang 
                      link
                      fix))
    (set-eval-limits ev 2 200)
    ev))

(define the-pure-eval
   (make-monadic-eval '(monad@ δ@ alloc@ state@ ev@)
                      '(fix ev)))

(define the-pure-eval-alt
   (make-monadic-eval '(monad-alt@ δ@ alloc@ state@ ev@)
                      '(fix ev)))

(define the-trace-eval
   (make-monadic-eval '(ListO@ monad-output@ δ@ alloc@ state@ ev-trace@ ev@)
                      '(fix (ev-trace ev))))

(define the-reach-eval
   (make-monadic-eval '(PowerO@ monad-output@ δ@ alloc@ state@ ev-reach@ ev@)
                      '(fix (ev-reach ev))))

(define the-abs-delta-eval
  (make-monadic-eval '(monad-nd@ δ-abs@ alloc@ state@ ev@)
                     '(fix ev)))

(define the-0cfa-eval
   (make-monadic-eval '(monad-nd@ alloc-x@ state-nd@ δ-abs@ ev@) 
                      '(fix ev)))

(define the-simple-cache-eval
   (make-monadic-eval '(monad-cache@ state-nd@ alloc-x@ δ-abs@ ev@ ev-cache0@)
                      '(fix (ev-cache ev))))

(define the-pdcfa-eval
   (make-monadic-eval '(monad-pdcfa@ state-nd@ alloc-x@ δ-abs@ ev@ ev-cache@ eval-coind@)
                      '(eval-coind (fix (ev-cache ev)))))

(define the-symbolic-eval
   (make-monadic-eval '(monad-symbolic@ δ-symbolic@ alloc@ state@ ev-symbolic@ ev@)
                      '(fix (ev-symbolic ev))))
