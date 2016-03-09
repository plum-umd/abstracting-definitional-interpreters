#lang monadic-eval
(ev-base@ ev-trace@ monad-trace@ alloc-con@ delta-con@)
(fix (ev-trace ev))

(! ((ref 5) := 7))
