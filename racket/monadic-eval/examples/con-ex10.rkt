#lang monadic-eval
(ev-base@ monad-con@ alloc-con@ delta-con@)
(fix ev)

(! ((ref 5) := 7))
