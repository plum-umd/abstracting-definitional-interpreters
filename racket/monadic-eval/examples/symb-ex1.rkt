#lang monadic-eval
(ev-base@ monad-symbolic@ alloc-con@ delta-symbolic@ ev-symbolic@)
(fix (ev-symbolic ev))
((λ (n) (if0 n (add1 3) 8)) 0)
