#lang monadic-eval
(ev@ ev-ref@ ev-reach@ monad-reach@ alloc@ δ@)
(fix (ev-reach (ev-ref ev)))

(! (ref 5))
