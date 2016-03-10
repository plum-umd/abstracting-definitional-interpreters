#lang monadic-eval
(ev-ref@ ev-reach@ monad-reach@ alloc@ δ@ ev@)
(fix (ev-reach (ev-ref ev)))

(! (ref 5))
