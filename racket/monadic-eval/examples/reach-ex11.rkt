#lang monadic-eval
(ev@ ev-ref@ ev-reach@ monad-reach@ alloc@ δ@)
(fix (ev-reach (ev-ref ev)))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
