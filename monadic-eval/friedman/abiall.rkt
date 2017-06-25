#lang racket

;; Code from the Friedman and Mendhekar tutorial

(define Ds-zero?
  (λ (x)
    (match x
      ['⊥ '⊥]
      [(? integer? x) (zero? x)]
      [_ '⊤])))

(define Ds-plus
  (λ (x y)
    (match* (x y)
      [('⊥ _) '⊥]
      [(_ '⊥) '⊥]
      [((? integer? x) (? integer? y))
       (cond
         [(or (zero? x) (zero? y)) (+ x y)]
         [(= x y) x]
         [else '⊤])]
      [(_ _) '⊤])))

(define Ds-minus
  (λ (x y)
    (match* (x y)
      [('⊥ _) '⊥]
      [(_ '⊥) '⊥]
      [(x (? integer? y)) (Ds-plus x (- y))]
      [(_ _) '⊤])))

(define Ds-times
  (λ (x y)
    (match* (x y)
      [('⊥ _) '⊥]
      [(_ '⊥) '⊥]
      [((? integer? x) (? integer? y))
       (* x y)]
      [(_ _) '⊤])))

(define Ds-int
  (λ (x)
    (cond
      [(positive? x) 1]
      [(negative? x) -1]
      [else 0])))

(define Ds-bool
  (λ (x) '⊥))

(define **hov-cache** #f)

(define cache-maker
  (λ (binary-predicate? not-found-value-constructor)
    (let ([cache '()])
      (λ (target)
        (letrec ([lookup (λ (table)
                           (cond
                             [(null? table)
                              (let ([value (not-found-value-constructor target)])
                                (set! cache (cons (mcons target value) cache))
                                value)]
                             [(binary-predicate? target (mcar (car table)))
                              (mcdr (car table))]
                             [else (lookup (cdr table))]))])
          (lookup cache))))))

(define initialize-hov-cache
  (λ ()
    (set! **hov-cache** (cache-maker Ds-equiv? (λ (target) target)))))

(define Ds-lub
  (lambda (x y)
    (match* (x y)
      [(x y) #:when (Ds-equiv? x y) x]
      [('⊥ y) y]
      [(x '⊥) x]
      [((? integer? x) (? integer? y)) (Ds-base-lub x y)]
      [(`(hov ,cs1) `(hov ,cs2))
       (**hov-cache** (union cs1 cs2))]
      [(`(hov ,cs1) y) `(⊤ ,cs1)]
      [(x `(hov ,cs2)) `(⊤ ,cs2)]
      [(`(⊤ ,cs1) `(⊤ ,cs2)) `(⊤ ,(union cs1 cs2))]
      [('⊤ y) '⊤]
      [(x '⊤) '⊤]
      [(_ _) (error "Ds-lub")])))
      

(define Ds-equiv?
  (λ (x y)
    (match* (x y)
      [(x y) #:when (eq? x y) #t]
      [(`(hov ,cs1) `(hov ,cs2)) (set-equiv? cs1 cs2)]
      [(`(⊤ ,cs1) `(⊤ ,cs2)) (set-equiv? cs1 cs2)]
      [(_ _) #f])))

(define Ds-closure-equiv?
  (λ (cl-x cl-y)
    (match* (cl-x cl-y)
      [(`(closure ,xs1 ,e1 ,env1)
        `(closure ,xs2 ,e2 ,env2))
       (and (eq? e1 e2)
            (andmap (λ (var)
                      (Ds-equiv? (apply-env env1 var) (apply-env env2 var)))
                    (set-diff (free-vars e1) xs1)))])))

(define fix-maker
  (λ (binary-op predicate?)
    (λ (memo thunk)
      (letrec ([fix-loop (λ ()
                           (let ([value (binary-op (thunk) (mcdr memo))])
                             (cond
                               [(predicate? value (mcdr memo)) value]
                               [else
                                (begin
                                  (set-mcdr! memo value)
                                  (fix-loop))])))])
        (fix-loop)))))

(define potentially-apply-procedure
  (λ (eval)
    (λ (f vs)
      (match f
        [`(closure ,xs ,e ,env)
         (eval e (extend-env xs vs env))]
        [_ (error "Cannot apply")]))))

(define potentially-recursive-eval-maker
  (λ (D-int D-bool D-zero? D-plus D-minus D-times D-if D-closure D-apply-hov)
    (λ (eval)
      (λ (exp env)
        (match exp
          [(? integer? i) (D-int i)]
          [(? boolean? b) (D-bool b)]
          [(? symbol? x) (apply-env env x)]
          [`(zero? ,e1) (D-zero? (eval e1 env))]
          [`(+ ,e1 ,e2) (D-plus (eval e1 env) (eval e2 env))]
          [`(- ,e1 ,e2) (D-minus (eval e1 env) (eval e2 env))]
          [`(* ,e1 ,e2) (D-times (eval e1 env) (eval e2 env))]
          [`(if ,e1 ,e2 ,e3)
           ((D-if eval) e1 e2 e3 env)]
          [`(λ ,xs ,e) (D-closure `(closure ,xs ,e ,env))]
          [`(,e . ,es)
           ((D-apply-hov eval)
            (eval e env)
            (map (λ (x) (eval x env)) es))])))))

(define fix-finite
  (λ (potentially-recursive-eval)
    (let ([stack '()])
      (letrec ([eval (potentially-recursive-eval
                      (λ (exp env)
                        (cond
                          [(massq′ exp stack) => mcdr]
                          [else (let ([memo (mcons exp '⊥)]
                                      [thunk (λ () (eval exp env))])
                                  (set! stack (cons memo stack))
                                  (let ([value (fix-memo memo thunk)])
                                    (set! stack (cdr stack))
                                    value))])))])
        eval))))


(define fix-memo (fix-maker Ds-lub Ds-equiv?))

(define Ds-closure
  (λ (c)
    (**hov-cache** `(hov (,c)))))

(define Ds-lub-map
  (λ (f l)
    (cond
      [(null? l) '⊥]
      [else (Ds-lub (f (car l)) (Ds-lub-map f (cdr l)))])))


(define Ds-apply-hov
  (λ (eval)
    (let ([apply-procedure (potentially-apply-procedure eval)])
      (λ (hov args)
        (match hov
          ['⊥ '⊥]
          [`(hov ,cs)
           (Ds-lub-map (λ (proc)
                         (apply-procedure proc args))
                       cs)])))))

(define Ds-if
  (λ (eval)
    (λ (e1 e2 e3 env)
      (let ([test (eval e1 env)])
        (Ds-lub (eval e2 env) (eval e3 env))))))

(define Ds-eval
  (fix-finite
   (potentially-recursive-eval-maker
    Ds-int Ds-bool Ds-zero? Ds-plus Ds-minus Ds-times Ds-if Ds-closure Ds-apply-hov)))

;; -----
;; Not in paper but needed.

; like massq but for (immutable) lists of mutable pairs
(define (massq′ v lst)
  (cond [(empty? lst) #f]
        [(eq? (mcar (car lst)) v)
         (car lst)]
        [else
         (massq′ v (cdr lst))]))

(define (free-vars e)
  (match e
    [(? integer? i) '()]
    [(? boolean? b) '()]
    [(? symbol? x) (list x)]
    [`(zero? ,e1) (free-vars e1)]
    [`(+ ,e1 ,e2) (append (free-vars e1) (free-vars e2))]
    [`(- ,e1 ,e2) (append (free-vars e1) (free-vars e2))]
    [`(* ,e1 ,e2) (append (free-vars e1) (free-vars e2))]
    [`(if ,e1 ,e2 ,e3)
     (append (free-vars e1) (free-vars e2) (free-vars e3))]
    [`(λ ,xs ,e) (set-diff (free-vars e) xs)]
    [`(,e . ,es)
     (append (free-vars e) (apply append (map free-vars es)))]))

(define (extend-env xs vs env)
  (cons (map cons xs vs) env))

(define (apply-env env x)
  (cond [(assq x (car env)) => cdr]
        [else (apply-env (cdr env) x)]))

(define (set-diff xs ys) (remq* ys xs))

(define union append)

(define (set-equiv? x y)
  (and (andmap (λ (x1) (member x1 y Ds-closure-equiv?)) x)
       (andmap (λ (y1) (member y1 x Ds-closure-equiv?)) y)))


;; Fixme
;; Shouldn't matter -- all examples only in λ-calculus
(define (Ds-base-lub x y) '⊤)

;;-------

(define (run e)
  (initialize-hov-cache)
  (Ds-eval e '()))

;; Some examples

(define Y
  '(λ (f) ((λ (x) (f (λ (y) ((x x) y))))
           (λ (x) (f (λ (y) ((x x) y)))))))

(define potential-count ;; loops with an infinite number of closures
  '(λ (count)
     (λ (x)
       (count (λ (f) x)))))


(define zero
  `(λ (s) (λ (z) z)))

(define four
  '(λ (s)
     (λ (z)
       (s (s (s (s z)))))))

(define plus1
  '(λ (n)
     (λ (s)
       (λ (z)
         (s ((n s) z))))))

(define plus
  `(λ (n)
     (λ (m)
       ((n ,plus1) m))))

(define times
  `(λ (n)
     (λ (m)
       ((n (,plus m)) ,zero))))
  

; Notice this example doesn't suffer from the usual 0CFA problems
(run '((λ (f) ((f f) (λ (x) x))) (λ (y) y)))

(run `((,times ,four) ,four))
(run `((,Y ,potential-count) (λ (z) z)))
      