{-# LANGUAGE FlexibleContexts #-}

module Fixpoints.DeadCode where

import Control.Monad.State
import Data.List
import Util.ListSet

deadEval :: 
  ( MonadState (ListSet expr) m
  , Eq expr
  )
  => ((expr -> m (dom val)) -> expr -> m (dom val))
  -> expr 
  -> m (dom val)
deadEval eval expr = do
  modify $ liftListOp $ delete expr
  eval (deadEval eval) expr
