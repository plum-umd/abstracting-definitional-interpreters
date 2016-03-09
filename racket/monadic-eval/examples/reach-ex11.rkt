#lang monadic-eval
(ev-base@ ev-reach@ monad-reach@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix (ev-reach ev))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
