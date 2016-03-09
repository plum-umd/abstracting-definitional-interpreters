#lang monadic-eval
(ev-base@ monad-con@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(fix ev)

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
