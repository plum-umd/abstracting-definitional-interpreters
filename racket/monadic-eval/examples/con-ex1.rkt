#lang monadic-eval
(ev-base@ monad-con@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix ev)
((λ (n) (if0 n (add1 3) 8)) 0)
