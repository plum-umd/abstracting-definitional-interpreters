#lang monadic-eval
(ev-base@ ev-reach@ monad-reach@ alloc-con@ delta-con@)
(fix (ev-reach ev))

((λ (x) x) 5)
