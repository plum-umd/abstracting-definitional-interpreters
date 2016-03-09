#lang monadic-eval (ev@ monad@ alloc@ δ@) (fix ev)
((λ (n) (if0 n (add1 3) 8)) 0)
