#lang monadic-eval
(ev@ ev-ref@ monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@)
(fix (ev-symbolic (ev-ref ev)))

(! ((ref 5) := 7))
