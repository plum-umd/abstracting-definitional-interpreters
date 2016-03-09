#lang monadic-eval
(ev-base@ ev-trace@ monad-trace@ alloc-con@ delta-con@)
(fix (ev-trace ev))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
