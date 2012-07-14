#lang racket
(provide (all-defined-out))

(struct app (e0 e1)    #:transparent)
(struct lam (x  e)     #:transparent)
(struct vbl (x)        #:transparent)
(struct num (n)        #:transparent)
(struct ifz (e0 e1 e2) #:transparent)
(struct op1 (o e)      #:transparent)
(struct op2 (o e0 e1)  #:transparent)
(struct ref (e)        #:transparent)
(struct ubx (e)        #:transparent)
(struct sbx (e0 e1)    #:transparent)
(struct sym (s)        #:transparent)