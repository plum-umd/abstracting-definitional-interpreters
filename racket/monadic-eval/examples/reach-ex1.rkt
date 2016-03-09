#lang new-world
(ev-base@ ev-reach@ monad-reach@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@) (fix (ev-reach ev))
((λ (n) (if0 n (add1 3) 8)) 0)
