#lang monadic-eval
(ev@ ev-reach@ monad-reach@ alloc@ δ@)
(fix (ev-reach ev))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
