{-# LANGUAGE FlexibleContexts #-}

module Fixpoints.MemoEval where

import Control.Arrow
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe
import Monads
import Data.Lattice
import Util.MFunctor
import qualified Data.Map as Map

type MemoMap dom val expr env store = 
  Map (expr,env,store) (dom val)
type MemoTables dom val expr env store = 
  ( MemoMap dom val expr env store
  , MemoMap dom val expr env store
  )

memoEval ::
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
memoEval eval expr = do
  env <- askEnv
  store <- getStore
  let ss = (expr,env,store) 
  (m1,mx) <- get
  case Map.lookup ss m1 of
    Just vD -> return vD
    Nothing -> do
      modify $ first $ ljoin $ Map.singleton ss $ fromMaybe lbot $ Map.lookup ss mx
      eval (memoEval eval) expr
