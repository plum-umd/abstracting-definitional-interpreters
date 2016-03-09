#lang monadic-eval
(ev-base@ monad-symbolic@ alloc-con@ delta-symbolic@ ev-symbolic@)
(fix (ev-symbolic ev))

(add1 0)
