#lang monadic-eval
(ev-base@ ev-dead@ eval-dead@ monad-dead@ alloc-con@ delta-con@)
(eval-dead (fix (ev-dead ev)))

(+ 5 11)
