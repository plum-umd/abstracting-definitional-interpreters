#lang monadic-eval
(ev@ monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@)
(fix (ev-symbolic ev))
((λ (n) (if0 n (add1 3) 8)) 0)
