#lang racket
(require "../fix.rkt"
         "../units.rkt"
         "../syntax.rkt"
         "../tests/tests.rkt")
(provide eval eval/alloc-size eval/alloc-max eval/alloc-bang)

(define eval/alloc-size
  (let ()
    (define-values/invoke-unit/infer
      (link monad@ state@ alloc-size@ δ@ ev@ ev-debug@))
    (lambda (e)
      (mrun ((fix ev) e)))))

(define eval/alloc-max
  (let ()
    (define-values/invoke-unit/infer
      (link monad@ state@ alloc-max@ δ@ ev@ ev-debug@))
    (lambda (e)
      (mrun ((fix ev) e)))))

(define eval/alloc-bang
  (let ()
    (define-values/invoke-unit/infer
      (link monad@ state@ alloc-bang@ δ@ ev@ ev-debug@))
    (lambda (e)
      (mrun ((fix ev) e)))))

(define eval eval/alloc-bang)

