#lang monadic-eval
(ev-dead@ eval-dead@ monad-dead@ alloc@ δ@ ev!@)
(eval-dead (fix (ev-dead ev!)))

(add1 (if0 (! ((ref 0) := 1)) fail 42))
