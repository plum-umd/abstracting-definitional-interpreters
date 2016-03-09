#lang monadic-eval
(ev@ monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@)
(fix (ev-symbolic ev))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
