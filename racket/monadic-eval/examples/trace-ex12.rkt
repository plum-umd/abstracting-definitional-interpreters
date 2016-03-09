#lang monadic-eval
(ev-base@ ev-trace@ monad-trace@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix (ev-trace ev))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
