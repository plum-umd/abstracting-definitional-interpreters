#lang new-world
(ev-base@ monad-con@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix ev)

(add1 (if0 (! ((ref 0) := 1)) fail 42))
