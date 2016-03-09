#lang monadic-eval
(ev-base@ ev-reach@ monad-reach@ alloc-con@ delta-con@) (fix (ev-reach ev))
((λ (n) (if0 n (add1 3) 8)) 0)
