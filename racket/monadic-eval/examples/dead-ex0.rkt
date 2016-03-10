#lang monadic-eval
(ev-dead@ eval-dead@ monad-dead@ alloc@ δ@ ev!@)
(eval-dead (fix (ev-dead ev!)))

(add1 0)
