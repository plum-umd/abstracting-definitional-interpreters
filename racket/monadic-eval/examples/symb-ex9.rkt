#lang monadic-eval
(monad-symbolic@ alloc@ state@ δ-symbolic@ ev-symbolic@ ev!@)
(fix (ev-symbolic ev!))

(! (ref 5))
