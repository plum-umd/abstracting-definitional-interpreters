#lang monadic-eval
(ev-base@ monad-con@ alloc-con@ delta-con@)
(fix ev)

((λ (x) (λ (_) x)) 5)
