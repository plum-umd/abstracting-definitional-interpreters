#lang monadic-eval
(ev-ref@ ev-reach@ monad-reach@ alloc@ δ@ ev@)
(fix (ev-reach (ev-ref ev)))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
