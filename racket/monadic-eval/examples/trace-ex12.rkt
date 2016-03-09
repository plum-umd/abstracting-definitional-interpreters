#lang monadic-eval
(ev@ ev-ref@ ev-trace@ monad-trace@ alloc@ δ@)
(fix (ev-trace (ev-ref ev)))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
