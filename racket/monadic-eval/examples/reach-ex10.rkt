#lang monadic-eval
(ev-base@ ev-reach@ monad-reach@ alloc-con@ delta-con@)
(fix (ev-reach ev))

(! ((ref 5) := 7))
