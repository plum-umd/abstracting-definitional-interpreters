#lang monadic-eval
(monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@ ev!@)
(fix (ev-symbolic ev!))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
