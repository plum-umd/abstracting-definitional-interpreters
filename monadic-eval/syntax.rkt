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
