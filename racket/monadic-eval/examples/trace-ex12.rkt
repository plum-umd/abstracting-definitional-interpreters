#lang monadic-eval
(ev@ ev-trace@ monad-trace@ alloc@ δ@)
(fix (ev-trace ev))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
