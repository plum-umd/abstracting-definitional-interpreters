#lang monadic-eval (link pdcfa-dead@ ev-symbolic@ abs-δ@ sto-0cfa@)

(if0 7 1 2)        ; The 1 is dead
(if0 (add1 7) 1 2) ; Nothing is dead


(rec f (λ (x)
         (if0 x
              1
              (* x (f (sub1 x)))))
  (f 5))
