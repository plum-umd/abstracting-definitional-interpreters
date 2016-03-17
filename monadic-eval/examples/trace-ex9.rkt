#lang monadic-eval
(ListO@ monad-output@ alloc@ state@ δ@ ev-trace@ ev!@)
(fix (ev-trace ev!))

(! (ref 5))
