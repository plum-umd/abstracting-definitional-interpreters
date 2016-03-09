#lang monadic-eval (ev-ref@  monad@ alloc@ δ@ ev@) (fix (ev-ref ev))

(! ((ref 5) := 7))
