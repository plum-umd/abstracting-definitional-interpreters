#lang monadic-eval (monad@ alloc@ state@ δ@ ev!@) (fix ev!)

(add1 (if0 (! ((ref 0) := 1)) fail 42))
