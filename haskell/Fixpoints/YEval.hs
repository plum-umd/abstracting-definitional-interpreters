module Fixpoints.YEval where

import Classes

yEval :: ( MonadEnv addr m, MonadStore dom addr val m, MonadTime time m
         , Addressable addr time
         , Promote dom m, Pointed dom
         , Ord addr) 
       => ((expr -> m (val addr)) -> (expr -> m (val addr)))
       -> (expr -> m (val addr))
yEval eval = 
  let f = eval (yEval eval)
  in f
