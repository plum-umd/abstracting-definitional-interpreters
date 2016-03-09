#lang monadic-eval
(ev@ ev-ref@ ev-dead@ eval-dead@ monad-dead@ alloc@ δ@)
(eval-dead (fix (ev-dead (ev-ref ev))))

(! (ref 5))
