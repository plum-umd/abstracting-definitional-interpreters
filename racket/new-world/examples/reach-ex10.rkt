#lang new-world
(ev-base@ ev-reach@ monad-reach@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix (ev-reach ev))

(! ((ref 5) := 7))