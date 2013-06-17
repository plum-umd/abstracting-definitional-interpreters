module Fixpoints.YEval where

yEval :: ((expr -> m a) -> expr -> m a)
      -> expr 
      -> m a
yEval eval = eval (yEval eval)
