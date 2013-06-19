{-# LANGUAGE FlexibleContexts #-}

module Fixpoints.Y where

import StateSpace.Semantics
import Control.Monad.Identity

fixY :: ((expr -> m a) -> expr -> m a)
      -> expr 
      -> m a
fixY eval = eval (fixY eval)

runY :: 
     ( BigStep t
     , Inject (InBS t)
     )
  => ((a -> t Identity b) -> a -> t Identity b)
  -> a
  -> OutBS t b
runY eval e = runIdentity $ runBS (fixY eval) (inject e)
