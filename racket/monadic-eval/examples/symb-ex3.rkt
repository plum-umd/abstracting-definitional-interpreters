#lang monadic-eval
(ev@ monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@)
(fix (ev-symbolic ev))

(+ 5 11)
