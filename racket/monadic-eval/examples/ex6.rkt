#lang monadic-eval (ev@ monad@ alloc@ δ@) (fix ev)

((λ (x) (λ (_) x)) 5)
