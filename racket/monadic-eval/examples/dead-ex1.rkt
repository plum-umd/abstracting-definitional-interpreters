#lang monadic-eval
(ev@ ev-dead@ eval-dead@ monad-dead@ alloc@ δ@)
(eval-dead (fix (ev-dead ev)))
((λ (n) (if0 n (add1 3) 8)) 0)
