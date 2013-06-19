#lang monadic-eval (link pdcfa-dead@ ev-symbolic@ abs-δ@ sto-0cfa@)
(if0 7 1 2)        ; The 1 is dead
(if0 (add1 7) 1 2) ; Nothing is dead

((λ (f) ((λ (y) (f 5)) (f 6)))
 (λ (x) x)) ; Nat, unlike {5, 6} of sto-0cfa.

((λ (f) (f 1))
 (λ (x) (add1 x))) ; {2}, unlike Nat of sto-0cfa.

(rec f (λ (x)         
         (if0 x
              1
              ;; NB: this order causes a blur by the time
              ;; we get to second x occurrence.
              (* (f (sub1 x)) x)
              ;; This order returns before blurring
              ;; so you get
              ;; (* 5 (f 4))
              ;; which leads to x -> Nat
              ;; so (* 5 (f 4)) = {1} or Nat
              ;; hence result is {5, Nat}.
              #;
              (* x (f (sub1 x)))))
  (f 5))

;; Example of need for PDCFA and abstract GC
;; from ICFP'12
(rec id (λ (x) x)
  (rec f (λ (n)
           (if0 n ; n <= 0 in ICFP'12
                1
                (* n (f (sub1 n)))))
    (rec g (λ (m)
             (if0 m
                  1
                  (+ (* m m) (g (sub1 m)))))
      (+ ((id f) 3)
         ((id g) 4)))))

(rec id (λ (x) x)
  (rec left (λ (l) (λ (r) l))
    ((left (id 1)) (id 2)))) ; {1}

(rec id (λ (x) x)
  (rec right (λ (l) (λ (r) r))
    ((right (id 1)) (id 2)))) ; {1,2}
    