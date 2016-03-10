#lang monadic-eval
(ev-reach@ monad-reach@ alloc@ δ@ ev!@)
(fix (ev-reach ev!))

(! ((ref 5) := 7))
