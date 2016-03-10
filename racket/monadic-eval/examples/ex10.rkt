#lang monadic-eval ( monad@ alloc@ δ@ ev!@) (fix ev!)

(! ((ref 5) := 7))
