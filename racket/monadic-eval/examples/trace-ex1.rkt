#lang monadic-eval
(ev@ ev-trace@ monad-trace@ alloc@ δ@)
(fix (ev-trace ev))
((λ (n) (if0 n (add1 3) 8)) 0)
