#lang monadic-eval
(ev-ref@ ev-dead@ eval-dead@ monad-dead@ alloc@ δ@ ev@)
(eval-dead (fix (ev-dead (ev-ref ev))))

(! (ref 5))
