#lang monadic-eval
(ev-base@ monad-symbolic@ alloc-con@ delta-symbolic@ ev-symbolic@)
(fix (ev-symbolic ev))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
