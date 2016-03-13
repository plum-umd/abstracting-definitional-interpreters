#lang racket
(provide (all-defined-out))
(require "sstruct.rkt")

;; [exp] is a core functional language with higher-order functions
;; and numbers:
;;   exp?   : (or app? lam? vbl? num? ifz? op1? op2? lrc?)
;;
;; [exp!] extends [exp] with mutable references:
;;   exp!?  : (or exp? ref? drf? srf?)
;;
;; [sexp!] extends [exp!] with holes for symbolic execution
;;   sexp!? : (or exp!? sym?)

(struct app (e0 e1)    #:symbolic)
(struct lam (x  e)     #:symbolic)
(struct vbl (x)        #:symbolic)
(struct num (n)        #:symbolic)
(struct ifz (e0 e1 e2) #:symbolic)
(struct op1 (o e)      #:symbolic)
(struct op2 (o e0 e1)  #:symbolic)
(struct lrc (f x e)    #:symbolic)

(struct ref (e)        #:symbolic)
(struct drf (e)        #:symbolic)
(struct srf (e0 e1)    #:symbolic)

(struct sym (s)        #:symbolic)

(define (pp s)
  (match s
    [(app (lam x b) a) (format "(let ~a ~a ~a)" x (pp a) (pp b))]
    [(app e0 e1) (format "(~a ~a)" (pp e0) (pp e1))]
    [(lam x  e1) (format "(λ (~a) ~a)" x (pp e1))]
    [(vbl x)     (format "~a" x)]
    [(num n)     (format "~a" n)]
    [(ifz e0 e1 e2) (format "(if0 ~a ~a ~a)" (pp e0) (pp e1) (pp e2))]
    [(op1 op e)     (format "(~a ~a)" op (pp e))]
    [(op2 op e0 e1) (format "(~a ~a ~a)" op (pp e0) (pp e1))]
    [(lrc f x e)    (format "(rec ~a ~a ~a)" f (pp x) (pp e))]
    [(ref e)        (format "(ref ~a)" (pp e))]
    [(drf e)        (format "(! ~a)" (pp e))]
    [(srf e0 e1)    (format "(~a := ~a)" (pp e0) (pp e1))]
    [(sym s)        (format "'~a" s)]))
