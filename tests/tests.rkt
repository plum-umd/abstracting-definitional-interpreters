#lang racket
(require rackunit
         racket/engine
         racket/match
         racket/set
         (for-syntax racket/list racket/string syntax/parse racket/set)
         "../map.rkt"
         "../parser.rkt"
         "../syntax.rkt")
(provide test dd dd* fact omega omega-push ref-sref)

(define (dd N)
  ;; returns 2 if N=0, 13 otherwise
  (parse
   `(let input ,N
      (let x (if0 input (if0 input 2 3) (if0 input 5 7))
        (let y (if0 input 11 13)
          (if0 input x y))))))

(define (dd* N)
  ;; returns 22 if N=0, 91 otherwise
  (parse
   `(let input ,N
      (let x (if0 input (if0 input 2 3) (if0 input 5 7))
        (let y (if0 input 11 13)
          (* x y))))))

(define (fact N)
  ;; DIVERGES when N < 0
  (parse
   `(rec f (λ (x)
             (if0 x
                  1
                  (* x (f (- x 1)))))
         (f ,N))))

(define omega
  ;; DIVERGES
  (parse `(let U (λ (f) (f f))
            (U U))))

(define omega-push
  ;; DIVERGES and continually pushes on new stack frames
  (parse `(let Uₚ (λ (f) (f (f f)))
            (Uₚ Uₚ))))

(define ref-sref
  (parse '(if0 (! ((ref 1) := 0)) 42 err)))

(define-for-syntax MAXTIME 5)
(define (timeout secs thunk)
  (define e (engine (λ (_) (thunk))))
  (if (engine-run (* secs 1000) e)
      (engine-result e)
      #f))

(define (equish? pair pair′)
  (match* (pair pair′)
    [(`(_ ,x) `(,_ ,y)) (equal? x y)]
    [(`(,x _) `(,y ,_)) (equal? x y)]
    [(`(,_ ,x) `(_ ,y)) (equal? x y)]
    [(`(,x ,_) `(,y _)) (equal? x y)]
    [(p q)              (equal? p q)]))

(define (truish? x) (not (false? x)))

(define-syntax (test stx)
  (syntax-parse stx
    #:datum-literals (DIVERGES)
    ;; Tests for divergence, assuming no tests
    ;; runs longer than MAXTIME seconds.
    [(test eval:expr exp:expr DIVERGES
           (~optional (~seq #:timeout max-time:expr)
                      #:defaults ([max-time #`#,MAXTIME])))
     (with-syntax ([test-col  (syntax-column #'eval)]
                   [test-line (syntax-line #'eval)]
                   [test-file (string-join
                               (take-right
                                (string-split
                                 (format "~a" (syntax-source #'eval))
                                 #rx"/|>")
                                2)
                               "/")])
       #'(with-check-info (('name   'eval)
                           ('params (pp exp))
                           ('tests  test-file)
                           ('location (format "line ~a, col ~a"
                                              test-line test-col)))
           (check-false (timeout max-time (λ () (eval exp))))))]

    ;; Tests that the output values are what we expect,
    ;; and the store has at least the given values σ-v ...
    ;; in its codomain. `get-as.σs' takes the raw output
    ;; from the evaluator and returns a list of pairs
    ;; with answers on the left and a store
    ;; on the right.
    [(test eval:expr exp:expr get-as-σs
           (~optional (~seq #:timeout max-time:expr)
                      #:defaults ([max-time #`#,MAXTIME]))
           (~seq (~optional (~seq #:answer exp-a:expr)
                            #:defaults ([exp-a #'{set}]))
                 #:bindings exp-b:expr ...) ...
                 (~seq #:preds pred) ...)
     (with-syntax ([test-col  (syntax-column #'eval)]
                   [test-line (syntax-line #'eval)]
                   [test-file (string-join
                                (take-right
                                  (string-split
                                    (format "~a" (syntax-source #'eval))
                                    #rx"/|>")
                                  2)
                                "/")])
       #'(with-check-info (('name   'eval)
                           ('params (pp exp))
                           ('tests  test-file)
                           ('location (format "line ~a, col ~a"
                                              test-line test-col)))
           (let ([out (timeout max-time (λ () (eval exp)))])
             ;; if output is #f, it ran longer than MAXTIME
             ;; and we assume it diverged
             (check-true (truish? out)) ;; diverged?
             (if (not out)
                 (begin
                   (printf "Test ran for more than ~a seconds.\n~a\n"
                           max-time
                           (pp exp))
                   (printf "We assume it diverged.\n"))

                 (let* ([σ-to-bs (λ (σ) (for/set ([(k v) (∈ σ)])
                                          (list (if (string? k)
                                                    (string-trim k #px"_.*")
                                                    k)
                                                v)))]
                        [act-as.bs (for/set ([as.σ (get-as-σs out)])
                                     (cons (car as.σ) (σ-to-bs (cdr as.σ))))]
                        [exp-as.bs (set (cons exp-a
                                               (set exp-b ...)) ...)])
                   ;(printf "Expected:\n~a\n" exp-as.bs)
                   ;(printf "Actual:\n~a\n" act-as.bs)
                   ;(printf "Raw out:\n~a\n" out)
                   (let ([unexpected-results
                          (for/fold ([rem-act act-as.bs])
                                    ([exp-a.b exp-as.bs])
                            (let* ([e-a (car exp-a.b)]
                                   [e-b (cdr exp-a.b)]
                                   [rem-act′
                                    (for/first ([act-a.b rem-act]
                                                #:when
                                                (and (equish? e-a (car act-a.b))
                                                     (for/and ([a-b (cdr act-a.b)])
                                                       (for/or ([eb e-b])
                                                         (equish? eb a-b)))))
                                      (set-remove rem-act act-a.b))])
                              (if rem-act′ rem-act′
                                  (begin
                                    (check-true #f)
                                    (printf "Did not compute expected result:")
                                    (printf "\nas: ~a\nσ:  ~a\n" e-a e-b)
                                    rem-act))))])
                     (check-true (set-empty? unexpected-results))
                     (for ([unexp unexpected-results])
                       (printf "Unexpected result:\nas: ~a\nσ:  ~a\n"
                               (car unexp) (cdr unexp)))
                     (check-true (pred out)) ...))))))]))
