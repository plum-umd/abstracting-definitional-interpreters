#lang new-world
(ev-base@ ev-dead@ eval-dead@ monad-dead@ alloc-nat@ delta-con@ ref-explicit@ st-explicit@)
(eval-dead (fix (ev-dead ev)))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
