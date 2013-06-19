{-# LANGUAGE QuasiQuotes #-}

module Lang.Lambda.Parsing where

import Lang.Lambda.AST
import Data.SExp (SExp,sexp)

-- from SExp conversion --

eFromS :: SExp -> Expr
eFromS [sexp| @int:n |] = LitE n
eFromS [sexp| @sym:x |] = VarE x
eFromS [sexp| (lam (@sym:x) @:body) |] = LamE x (eFromS body)
eFromS [sexp| (let (@sym:x @:e1) @:e2) |] = LetE x (eFromS e1) (eFromS e2)
eFromS [sexp| (letrec (@sym:f (@sym:x) @:body) @:e) |] = LetRecE f x (eFromS body) (eFromS e)
eFromS [sexp| (if0 @:c @:tb @:fb) |] = IfZE (eFromS c) (eFromS tb) (eFromS fb)
eFromS [sexp| (add1 @:e) |] = PrimE Add1 [eFromS e]
eFromS [sexp| (sub1 @:e) |] = PrimE Sub1 [eFromS e]
eFromS [sexp| (@:e1 @:e2) |] = AppE (eFromS e1) (eFromS e2)
eFromS [sexp| (@:e1 @:e2 . @:es) |] = eFromS [sexp| ((@:e1 @:e2) . @:es) |]
eFromS _ = error "could not convert from S"

aFromS :: SExp -> Atom
aFromS [sexp| @int:n |] = LitA n
aFromS [sexp| @sym:x |] = VarA x
aFromS [sexp| (lam (@sym:x @sym:k) @:body) |] = LamA x k (cFromS body)
aFromS [sexp| (kon (@sym:x) @:body) |] = KonA x (cFromS body)
aFromS [sexp| (add1 @:a) |] = PrimA Add1 [aFromS a]
aFromS [sexp| (sub1 @:a) |] = PrimA Sub1 [aFromS a]
aFromS _ = error "could not convert from S"

cFromS :: SExp -> Call
cFromS [sexp| (let (@sym:x @:a) @:c) |] = LetC x (aFromS a) (cFromS c)
cFromS [sexp| (letrec (@sym:f (@sym:x @sym:k) @:body) @:c) |] = LetRecC f x k (cFromS body) (cFromS c)
cFromS [sexp| (if0 @:ca @:tb @:fb) |] = IfZC (aFromS ca) (cFromS tb) (cFromS fb)
cFromS [sexp| (<halt> @:a) |] = HaltC (aFromS a)
cFromS [sexp| (@:a1 @:a2 @:a3) |] = LAppC (aFromS a1) (aFromS a2) (aFromS a3)
cFromS [sexp| (@:a1 @:a2) |] = KAppC (aFromS a1) (aFromS a2)
cFromS _ = error "could not convert from S"
