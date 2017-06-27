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
(struct lrc (f e)      #:symbolic)

(struct ref (e)        #:symbolic)
(struct drf (e)        #:symbolic)
(struct srf (e0 e1)    #:symbolic)

(struct sym (s)        #:symbolic)


(define (fv e)
  (match e
    [(app e0 e1) (set-union (fv e0) (fv e1))]
    [(lam x e) (set-remove (fv e) x)]
    [(vbl x) (set x)]
    [(num n) (set)]
    [(ifz e0 e1 e2)
     (set-union (fv e0)
                (fv e1)
                (fv e2))]
    [(op1 o e)
     (fv e)]
    [(op2 o e0 e1)
     (set-union (fv e0)
                (fv e1))]
    [(ref e)
     (fv e)]
    [(drf e)
     (fv e)]
    [(srf e0 e1)
     (set-union (fv e0) (fv e1))]
    [(sym s) (set)]
    [(lrc f e)
     (set-remove (fv e) f)]
    [_ (set)]))

(define (subexps e)
  (set-add (match e
             [(or 'err
                  (vbl _)
                  (sym _)
                  (num _))
              (set)]
             [(or (op1 _ e)
                  (ref e)
                  (drf e)
                  (lrc _ e)
                  (lam _ e))
              (subexps e)]
             [(or (op2 _ e0 e1)
                  (srf e0 e1)
                  (app e0 e1))
              (set-union (subexps e0)
                         (subexps e1))]
             [(ifz e0 e1 e2)
              (set-union (subexps e0)
                         (subexps e1)
                         (subexps e2))])
           e))
