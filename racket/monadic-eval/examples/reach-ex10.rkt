#lang monadic-eval
(ev@ ev-reach@ monad-reach@ alloc@ δ@)
(fix (ev-reach ev))

(! ((ref 5) := 7))
