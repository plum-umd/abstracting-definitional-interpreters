#lang monadic-eval
(ev-reach@ monad-reach@ alloc@ state@ δ@ ev!@)
(fix (ev-reach ev!))

(! (ref 5))
