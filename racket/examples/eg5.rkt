#lang monadic-eval (link pdcfa@ ev-symbolic@ abs-δ@ sto-0cfa@)
(if0 (add1 7) 1 2)

(rec f (λ (x)
         (if0 x
              1
              (* x (f (sub1 x)))))
  (f 5))


(rec f (λ (x)
         (if0 x
              (f (add1 x))
              (f (add1 x))))
  (f 2))
