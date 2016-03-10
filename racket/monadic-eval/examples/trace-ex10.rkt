#lang monadic-eval
(ev-trace@ monad-trace@ alloc@ δ@ ev!@)
(fix (ev-trace ev!))

(! ((ref 5) := 7))
