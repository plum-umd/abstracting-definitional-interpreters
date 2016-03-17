#lang racket

(module+ test
  (require rackunit
           "../map.rkt"
           "../fixed/eval-cycle.rkt"
           "../syntax.rkt"
           "tests.rkt")

  ;; eval : e → ℘(v × σ)  ; nondet only used as an option here
  (define (get-v-σs x) x)

  (test eval (dd 0) get-v-σs
        #:answer   2
        #:bindings '("input" 0) '("x" 2) '("y" 11))

  (test eval (dd 1) get-v-σs
        #:answer   13
        #:bindings '("input" 1) '("x" 7) '("y" 13))

  (test eval (dd* 0) get-v-σs
        #:answer   22
        #:bindings '("input" 0) '("x" 2) '("y" 11))

  (test eval (dd* 1) get-v-σs
        #:answer   91
        #:bindings '("input" 1) '("x" 7) '("y" 13))

  (test eval (fact 5) get-v-σs
        #:answer   120
        #:bindings '("x" 0) '("f" _))

  (test eval (fact -1) DIVERGES)

  (test eval omega get-v-σs)

  (test eval omega-push get-v-σs)

  (test eval ref-sref get-v-σs
        #:answer   42
        #:bindings '(_ 0)))
