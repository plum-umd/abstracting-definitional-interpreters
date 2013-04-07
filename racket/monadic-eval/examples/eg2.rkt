#lang monadic-eval (link eval-pure@ ev@ δ@ env@)
(rec f (λ (x) (f x)) f)
