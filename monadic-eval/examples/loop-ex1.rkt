#lang monadic-eval
(ev-loop@ state-crush@ δ-pres@ ev!@)
(fix (ev-loop ev!))

(let ([kons (λ (x)
            (λ (y)
              (λ (m) (if0 m x y))))])
  (let ([kar (λ (p) (p 0))])
    (let ([kdr (λ (p) (p 1))])
      (let ([kadr (λ (p) (kar (kdr p)))])
        (kadr ((kons 1) ((kons 2) ((kons 3) 0))))))))
