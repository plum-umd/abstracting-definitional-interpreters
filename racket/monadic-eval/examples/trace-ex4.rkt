#lang monadic-eval
(ev-base@ ev-trace@ monad-trace@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix (ev-trace ev))

(λ (x) x)
