#lang monadic-eval
(ev-loop@ state-crush@ δ-pres@ ev!@)
(fix (ev-loop ev!))

(let try
    (λ (f) (if0 (f 1) 0 (f 0))) ; sat(f) ≡ ∃x.f(x) = 0
  (let sat-solve-7
      (λ (p)
        (try (λ (n₁)
               (try (λ (n₂)
                      (try (λ (n₃)
                             (try (λ (n₄)
                                    (try (λ (n₅)
                                           (try (λ (n₆)
                                                  (try (λ (n₇)
                                                         (((((((p n₁) n₂) n₃) n₄) n₅) n₆) n₇))))))))))))))))
    (let φ
      (λ (x₁)
        (λ (x₂)
          (λ (x₃)
            (λ (x₄)
              (λ (x₅)
                (λ (x₆)
                  (λ (x₇) ; (and0 x₁ .. xₙ), worst case in terms of branching
                    (if0 x₁
                         (if0 x₂
                              (if0 x₃
                                   (if0 x₄
                                        (if0 x₅
                                             (if0 x₆
                                                  (if0 x₇ 0 1)
                                                  1)
                                             1)
                                        1)
                                   1)
                              1)
                         1))))))))
      (sat-solve-7 φ))))
