#lang monadic-eval
(ev@ monad-symbolic@ alloc@ δ-symbolic@ ev-symbolic@)
(fix (ev-symbolic ev))

((λ (x) (λ (_) x)) 5)
