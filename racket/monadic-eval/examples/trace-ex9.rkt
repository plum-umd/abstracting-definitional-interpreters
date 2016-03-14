#lang monadic-eval
(monad-trace@ alloc@ state@ δ@ ev-trace@ ev!@)
(fix (ev-trace ev!))

(! (ref 5))
