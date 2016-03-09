#lang new-world
(ev-base@ ev-dead@ eval-dead@ monad-dead@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(eval-dead (fix (ev-dead ev)))

(! ((ref 5) := 7))
