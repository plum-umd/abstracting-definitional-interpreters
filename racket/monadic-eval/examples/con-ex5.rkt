#lang monadic-eval
(ev-base@ monad-con@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix ev)

((λ (x) x) 5)
