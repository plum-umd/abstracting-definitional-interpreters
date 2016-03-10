#lang monadic-eval
(ev-trace@ monad-trace@ alloc@ state@ δ@ ev!@)
(fix (ev-trace ev!))

(! (ref 5))
