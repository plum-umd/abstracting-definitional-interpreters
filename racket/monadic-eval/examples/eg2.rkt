#lang monadic-eval (link eval-pure@ ev@ δ@ env@ err@)
(rec f (λ (x) (f x)) f)
