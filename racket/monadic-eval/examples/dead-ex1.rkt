#lang monadic-eval
(ev-dead@ eval-dead@ monad-dead@ alloc@ state@ δ@ ev!@)
(eval-dead (fix (ev-dead ev!)))
((λ (n) (if0 n (add1 3) 8)) 0)
