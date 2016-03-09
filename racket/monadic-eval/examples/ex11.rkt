#lang monadic-eval (ev-ref@ monad@ alloc@ δ@ ev@) (fix (ev-ref ev))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
