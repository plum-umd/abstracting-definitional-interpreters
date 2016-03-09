#lang monadic-eval
(ev-ref@ ev-trace@ monad-trace@ alloc@ δ@ ev@)
(fix (ev-trace (ev-ref ev)))

(! ((ref 5) := 7))
