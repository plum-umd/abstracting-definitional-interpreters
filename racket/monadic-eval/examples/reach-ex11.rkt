#lang monadic-eval
(ev-reach@ monad-reach@ alloc@ state@ δ@ ev!@)
(fix (ev-reach ev!))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
