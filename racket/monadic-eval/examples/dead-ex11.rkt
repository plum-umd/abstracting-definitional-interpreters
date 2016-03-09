#lang monadic-eval
(ev@ ev-ref@ ev-dead@ eval-dead@ monad-dead@ alloc@ δ@)
(eval-dead (fix (ev-dead (ev-ref ev))))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
