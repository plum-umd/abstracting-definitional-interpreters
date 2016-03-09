#lang monadic-eval
(ev@ ev-reach@ monad-reach@ alloc@ δ@) (fix (ev-reach ev))
((λ (n) (if0 n (add1 3) 8)) 0)
