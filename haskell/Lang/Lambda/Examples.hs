{-# LANGUAGE QuasiQuotes #-}

module Lang.Lambda.Examples where

import Lang.Lambda.CPS
import Lang.Lambda.Data
import Lang.Lambda.Parsing
import Data.SExp

e1 :: Expr
e1 = eFromS
  [sexp|
    (let (id (lam (x) ((lam (q) q) x))) 
     (let (y (id 0))
      (let (z (id 1))
       y)))
  |] 

c1 :: Call
c1 = cps e1
