#lang monadic-eval
(ev@ ev-dead@ eval-dead@ monad-dead@ alloc@ δ@)
(eval-dead (fix (ev-dead ev)))

((λ (x) (λ (_) x)) 5)
