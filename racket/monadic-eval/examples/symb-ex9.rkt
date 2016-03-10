#lang monadic-eval
(monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@ ev!@)
(fix (ev-symbolic ev!))

(! (ref 5))
