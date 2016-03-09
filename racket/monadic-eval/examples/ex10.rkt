#lang monadic-eval (ev@ ev-ref@  monad@ alloc@ δ@) (fix (ev-ref ev))

(! ((ref 5) := 7))
