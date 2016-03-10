#lang monadic-eval
(ev-dead@ eval-dead@ monad-dead@ alloc@ state@ δ@ ev!@)
(eval-dead (fix (ev-dead ev!)))

(! ((ref 5) := 7))
