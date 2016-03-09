#lang monadic-eval
(ev-ref@ ev-trace@ monad-trace@ alloc@ δ@ ev@)
(fix (ev-trace (ev-ref ev)))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
