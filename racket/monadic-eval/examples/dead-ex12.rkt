#lang monadic-eval
(ev-dead@ eval-dead@ monad-dead@ alloc@ state@ δ@ ev!@)
(eval-dead (fix (ev-dead ev!)))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
