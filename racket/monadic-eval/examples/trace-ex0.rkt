#lang monadic-eval
(ev@ ev-trace@ monad-trace@ alloc@ δ@)
(fix (ev-trace ev))

(add1 0)
