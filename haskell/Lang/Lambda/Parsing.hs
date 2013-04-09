{-# LANGUAGE QuasiQuotes #-}

module Lang.Lambda.Parsing where

import Lang.Lambda.Data
import SExpKit

-- from SExp conversion --

eFromS :: SExp -> Expr
eFromS [sexp| @Lit$n |] = LitE n
eFromS [sexp| @Sym$x |] = VarE x
eFromS [sexp| (lam (@Sym$x) @$body) |] = LamE x (eFromS body)
eFromS [sexp| (let (@Sym$x @$e1) @$e2) |] = LetE x (eFromS e1) (eFromS e2)
eFromS [sexp| (letrec (@Sym$f (@Sym$x) @$body) @$e) |] = LetRecE f x (eFromS body) (eFromS e)
eFromS [sexp| (if0 @$c @$tb @$fb) |] = IfZE (eFromS c) (eFromS tb) (eFromS fb)
eFromS [sexp| (add1 @$e) |] = PrimE Add1 [eFromS e]
eFromS [sexp| (sub1 @$e) |] = PrimE Sub1 [eFromS e]
eFromS [sexp| (@$e1 @$e2) |] = AppE (eFromS e1) (eFromS e2)
eFromS [sexp| (@$e1 @$e2 . @$es) |] = eFromS [sexp| ((@$e1 @$e2) . @$es) |]
eFromS _ = error "could not convert from S"

aFromS :: SExp -> Atom
aFromS [sexp| @Lit$n |] = LitA n
aFromS [sexp| @Sym$x |] = VarA x
aFromS [sexp| (lam (@Sym$x @Sym$k) @$body) |] = LamA x k (cFromS body)
aFromS [sexp| (kon (@Sym$x) @$body) |] = KonA x (cFromS body)
aFromS [sexp| (add1 @$a) |] = PrimA Add1 [aFromS a]
aFromS [sexp| (sub1 @$a) |] = PrimA Sub1 [aFromS a]

cFromS :: SExp -> Call
cFromS [sexp| (let (@Sym$x @$a) @$c) |] = LetC x (aFromS a) (cFromS c)
cFromS [sexp| (letrec (@Sym$f (@Sym$x @Sym$k) @$body) @$c) |] = LetRecC f x k (cFromS body) (cFromS c)
cFromS [sexp| (if0 @$ca @$tb @$fb) |] = IfZC (aFromS ca) (cFromS tb) (cFromS fb)
cFromS [sexp| (<halt> @$a) |] = HaltC (aFromS a)
cFromS [sexp| (@$a1 @$a2 @$a3) |] = LAppC (aFromS a1) (aFromS a2) (aFromS a3)
cFromS [sexp| (@$a1 @$a2) |] = KAppC (aFromS a1) (aFromS a2)

