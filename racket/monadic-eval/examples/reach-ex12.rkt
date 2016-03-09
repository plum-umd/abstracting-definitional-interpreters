#lang monadic-eval
(ev-base@ ev-reach@ monad-reach@ alloc-con@ delta-con@)
(fix (ev-reach ev))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
