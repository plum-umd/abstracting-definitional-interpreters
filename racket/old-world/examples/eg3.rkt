#lang monadic-eval (link sval@ ev@ δ@ env-sto@)
;; Classical symbolic execution
;; Don't interpret test on if, but collect a verification condition
;; and evaluate both branches.
(add1 7)
(if0 (add1 7) 8 9)
(sub1 (if0 (add1 7) 8 9))

(rec f (λ (x)
         (if0 x
              1
              ;; calling f here makes this diverge
              (* x (sub1 x))))
  (f 0))

((λ (f) (f 3))
 (if0 5
      (λ (x) (sub1 x))
      (λ (y) (add1 y))))
