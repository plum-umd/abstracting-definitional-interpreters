#lang monadic-eval
(ev-base@ ev-trace@ monad-trace@ alloc-con@ delta-con@)
(fix (ev-trace ev))
((λ (n) (if0 n (add1 3) 8)) 0)
