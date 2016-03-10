#lang monadic-eval
(ev-ref@ monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@ ev@)
(fix (ev-symbolic (ev-ref ev)))

(! ((ref 5) := 7))
