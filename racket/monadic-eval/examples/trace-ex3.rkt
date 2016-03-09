#lang monadic-eval
(ev@ ev-trace@ monad-trace@ alloc@ δ@)
(fix (ev-trace ev))

(+ 5 11)
