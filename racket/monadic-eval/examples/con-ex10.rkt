#lang monadic-eval
(ev-base@ monad-con@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix ev)

(! ((ref 5) := 7))