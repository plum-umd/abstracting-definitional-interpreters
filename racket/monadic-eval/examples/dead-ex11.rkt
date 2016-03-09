#lang monadic-eval
(ev-base@ ev-dead@ eval-dead@ monad-dead@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(eval-dead (fix (ev-dead ev)))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
