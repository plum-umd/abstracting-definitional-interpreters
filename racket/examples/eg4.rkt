#lang monadic-eval (link eval-dead@ ev@ δ@ env-sto@)
;; Demonstrates computation of dead code
(rec f (λ (x)
         (if0 x
              1
              (* x (f (sub1 x)))))
  (f 0))
