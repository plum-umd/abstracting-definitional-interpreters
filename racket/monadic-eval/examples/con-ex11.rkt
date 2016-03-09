#lang monadic-eval
(ev-base@ monad-con@ alloc-con@ delta-con@)
(fix ev)

(add1 (if0 (! ((ref 0) := 1)) fail 42))
