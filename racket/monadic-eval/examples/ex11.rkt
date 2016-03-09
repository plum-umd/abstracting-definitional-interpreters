#lang monadic-eval (ev@ monad@ alloc@ δ@) (fix ev)

(add1 (if0 (! ((ref 0) := 1)) fail 42))
