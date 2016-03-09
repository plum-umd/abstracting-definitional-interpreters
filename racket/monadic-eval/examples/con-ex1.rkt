#lang monadic-eval
(ev-base@ monad-con@ alloc-con@ delta-con@)
(fix ev)
((λ (n) (if0 n (add1 3) 8)) 0)
