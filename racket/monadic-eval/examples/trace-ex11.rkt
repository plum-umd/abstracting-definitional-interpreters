#lang monadic-eval
(ListO@ monad-output@ alloc@ state@ δ@ ev-trace@ ev!@)
(fix (ev-trace ev!))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
