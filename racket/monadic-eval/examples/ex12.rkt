#lang monadic-eval (ev@ monad@ alloc@ δ@) (fix ev)

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
