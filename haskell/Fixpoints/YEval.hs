module Fixpoints.YEval where

yEval :: ((expr -> m a) -> (expr -> m a))
      -> (expr -> m a)
yEval eval = 
  let f = eval (yEval eval)
  in f
