{-# LANGUAGE QuasiQuotes #-}

module Lang.Lambda.Examples where

import Lang.Lambda.Data
import SExpKit

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
