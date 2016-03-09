#lang monadic-eval (ev@ ev-ref@ monad@ alloc@ δ@) (fix (ev-ref ev))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
