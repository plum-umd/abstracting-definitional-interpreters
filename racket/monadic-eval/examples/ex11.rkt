#lang monadic-eval (ev@ ev-ref@ monad@ alloc@ δ@) (fix (ev-ref ev))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
