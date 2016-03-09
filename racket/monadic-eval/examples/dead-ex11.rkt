#lang monadic-eval
(ev-base@ ev-dead@ eval-dead@ monad-dead@ alloc-con@ delta-con@)
(eval-dead (fix (ev-dead ev)))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
