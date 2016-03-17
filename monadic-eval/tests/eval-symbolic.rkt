#lang racket

(module+ test
  (require rackunit
           racket/set
           "../map.rkt"
           "../fixed/eval-symbolic.rkt"
           "../parser.rkt"
           "../syntax.rkt"
           "../transformers.rkt"
           "tests.rkt")

  ;; eval : exp -> ℘(((value ∪ (failure)) × σ) × φ)
  (define (get-as-σs out)
    (for/set ([a.σ.._ out])
      (cons (caar a.σ.._) (cdar a.σ.._))))

  (test eval (dd ''n) get-as-σs
        #:answer   13
        #:bindings '("input" n) '("x" 7) '("y" 13)
        #:answer   2
        #:bindings '("input" n) '("x" 2) '("y" 11))

  (test eval (dd 0) get-as-σs
        #:answer   2
        #:bindings '("input" 0) '("x" 2) '("y" 11))
  
  (test eval (dd 1) get-as-σs
        #:answer   13
        #:bindings '("input" 1) '("x" 7) '("y" 13))

  (test eval (dd* ''n) get-as-σs
        #:answer   91
        #:bindings '("input" n) '("x" 7) '("y" 13)
        #:answer   22
        #:bindings '("input" n) '("x" 2) '("y" 11))

  (test eval (dd* 0) get-as-σs
        #:answer   22
        #:bindings '("input" 0) '("x" 2) '("y" 11))
  
  (test eval (dd* 1) get-as-σs
        #:answer   91
        #:bindings '("input" 1) '("x" 7) '("y" 13))
  
  (test eval (fact 5) get-as-σs
        #:answer   120
        #:bindings '("x" 5) '("x" 4) '("x" 3) '("x" 2) '("x" 1) '("x" 0) '("f" _))
  
  (test eval (fact ''n) DIVERGES)
  
  (test eval (fact -1) DIVERGES)

  (define U (parse `(λ (f) (f f))))
  (test eval omega DIVERGES)

  (define Uₚ (parse `(λ (f) (f (f f)))))
  (test eval omega-push DIVERGES)

  (test eval ref-sref get-as-σs
        #:answer 42
        #:bindings `(_ 0))

  (define fail_42 (parse `(if0 's (quotient 1 's) 42)))
  (test eval fail_42 get-as-σs
        #:answer 42 #:bindings
        #:answer (failure) #:bindings)
  
  (define 1/s_42 (parse `(if0 (¬ 's) (quotient 1 's) 42)))
  (test eval 1/s_42 get-as-σs
        #:answer 42 #:bindings
        #:answer '(quotient 1 s) #:bindings)

  ;;   (check-eval (ifz (sym 's) (op2 'quotient (num 1) (sym 's)) (num 42))
;;               (failure)
;;               42)
;;   (check-eval (ifz (op1 'flip (sym 's)) (op2 'quotient (num 1) (sym 's)) (num 42))
;;               '(quotient 1 s)
;;               42))
  
  )

;; (require rackunit
;;          "../units.rkt"
;;          "../syntax.rkt"
;;          "../set.rkt")



;; (module+ test
;;   (check-eval (num 5) 5)
;;   (check-eval (op1 'add1 (num 5)) 6)
;;   (check-eval (op2 '+ (num 5) (num 11)) 16)
;;   (check-eval (lam 'x (vbl 'x))
;;               (cons (lam 'x (vbl 'x)) ρ))

;;   (check-eval (app (lam 'x (num 7)) (num 5)) 7)
;;   (check-eval (app (lam 'x (lam '_ (vbl 'x))) (num 5))
;;               (cons (lam '_ (vbl 'x)) ρ))            
;;   (check-eval (app (lam 'x (vbl 'x)) (num 5)) 5)          

;;   (check-eval (ifz (num 0) (num 7) (num 8)) 7)
;;   (check-eval (ifz (num 1) (num 7) (num 8)) 8)
;;   (check-eval (ref (num 5))
;;               (cons 'box _))
;;   (check-eval (drf (ref (num 5))) 5)
;;   (check-eval (drf (srf (ref (num 5)) (num 7))) 7)
;;   (check-eval (op1 'add1
;;                    (ifz (drf (srf (ref (num 0)) (num 1)))
;;                         (num 43)
;;                         (num 42)))
;;               43)
;;   (check-eval (op1 'add1
;;                    (ifz (drf (srf (ref (num 1)) (num 0)))
;;                         (num 43)
;;                         (num 42)))
;;               44)

;;   (check-eval (op1 'add1 (ifz (sym 'n) (num 7) (num 8)))
;;               8
;;               9)
            
;; ;; This is a type error and shouldn't be tested.
;; #;
;; (check-eval (op1 'add1 (app (sym 'f) (num '7)))
;;             (failure))
;;   (check-eval (lrc 'f (lam 'x
;;                            (ifz (vbl 'x)
;;                                 (vbl 'x)
;;                                 (op1 'add1
;;                                      (app (vbl 'f)
;;                                           (op2 '+ 
;;                                                (vbl 'x)
;;                                                (num -1))))))
;;                    (app (vbl 'f)
;;                         (num 5)))
;;               5)

;;   (check-eval (op2 'quotient (num 1) (num 0)) (failure))
;;   (check-eval (op2 'quotient (num 1) (sym 's)) (failure) '(quotient 1 s))
;;   (check-eval (op2 'quotient (sym 'n) (num 1)) '(quotient n 1))
;;   (check-eval (ifz (sym 'a)
;;                    (ifz (sym 'b) (num 1) (num 2))
;;                    (ifz (sym 'c) (num 3) (num 4)))
;;               1
;;               2
;;               3
;;               4)
;;   (check-eval (ifz (sym 's)
;;                    (ifz (sym 's) (num 1) (num 2))
;;                    (ifz (sym 's) (num 3) (num 4)))
;;               1
;;               4)
;;   (check-eval (ifz (op1 'flip (sym 's))
;;                    (ifz (sym 's) (num 1) (num 2))
;;                    (ifz (sym 's) (num 3) (num 4)))
;;               2
;;               3)
;;   (check-eval (ifz (sym 's)
;;                    (ifz (op1 'flip (sym 's)) (num 1) (num 2))
;;                    (ifz (op1 'flip (sym 's)) (num 3) (num 4)))
;;               2
;;               3)
;;   (check-eval (ifz (op1 'flip (sym 's))
;;                    (ifz (op1 'flip (sym 's)) (num 1) (num 2))
;;                    (ifz (op1 'flip (sym 's)) (num 3) (num 4)))
;;               1
;;               4)
;;   (check-eval (ifz (sym 's) (op2 'quotient (num 1) (sym 's)) (num 42))
;;               (failure)
;;               42)
;;   (check-eval (ifz (op1 'flip (sym 's)) (op2 'quotient (num 1) (sym 's)) (num 42))
;;               '(quotient 1 s)
;;               42))
