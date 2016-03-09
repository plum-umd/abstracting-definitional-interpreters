#lang monadic-eval
(ev@ ev-trace@ monad-trace@ alloc@ δ@)
(fix (ev-trace ev))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
