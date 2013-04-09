{-# LANGUAGE FlexibleContexts #-}

module Fixpoints.DeadMemoEval where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Fixpoints.MemoEval (MemoMap, MemoTables)
import Monads
import Util.Lattice
import Util.MFunctor
import qualified Data.Map as Map

deadMemoEval ::
  ( MonadEnvReader env m
  , MonadStoreState store m
  , MonadState (MemoTables dom val expr env store) m
  , MMorph dom m
  , Ord expr
  , Ord env
  , Ord store
  , Ord val
  , Lattice (dom val)
  )
  => ((expr -> m (dom val)) -> expr -> m (dom val))
  -> expr 
  -> m (dom val)
deadMemoEval eval expr = do
  -- HERE
  -- modify $ liftListOp $ List.delete expr
  env <- askEnv
  store <- getStore
  let ss = (expr,env,store) 
  (m1,mx) <- get
  case Map.lookup ss m1 of
    Just vD -> return vD
    Nothing -> do
      modify $ first $ ljoin $ Map.singleton ss $ fromMaybe lbot $ Map.lookup ss mx
      eval (deadMemoEval eval) expr

