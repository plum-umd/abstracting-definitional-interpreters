#lang monadic-eval
(ev-base@ ev-dead@ eval-dead@ monad-dead@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(eval-dead (fix (ev-dead ev)))
((λ (n) (if0 n (add1 3) 8)) 0)
