#lang monadic-eval
(ev-ref@ ev-dead@ eval-dead@ monad-dead@ alloc@ δ@ ev@)
(eval-dead (fix (ev-dead (ev-ref ev))))

(rec f (λ (x)
         (if0 x x
              (add1 (f (+ x -1)))))
     (f 5))
