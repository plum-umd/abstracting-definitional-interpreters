#lang monadic-eval
(ev@ ev-ref@ ev-trace@ monad-trace@ alloc@ δ@)
(fix (ev-trace (ev-ref ev)))

(! (ref 5))
