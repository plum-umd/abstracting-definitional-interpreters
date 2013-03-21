{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TupleSections, FlexibleContexts, ScopedTypeVariables #-}

module Fixpoints.MemoEval where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe
import Monads
import StateSpace
import Util
import qualified Data.Map as Map

type MemoMap dom val expr env store = 
  Map (expr,env,store) (dom val)
type MemoTables dom val expr env store = 
  ( MemoMap dom val expr env store
  , MemoMap dom val expr env store
  )

memoEval :: forall m dom val expr env store.
            ( MonadEnv env m
            , MonadStore store m
            , MonadState (MemoTables dom val expr env store) m
            , Promote dom m
            , Ord expr
            , Ord env
            , Ord store
            , Ord val
            , Lattice (dom val)) 
         => ((expr -> m val) -> (expr -> m val))
         -> (expr -> m val)
memoEval eval expr = do
  env <- askEnv
  store <- getStore
  let ss = (expr,env,store) 
  (m1,mx) <- get
  case Map.lookup ss m1 of
    Just vD -> promote vD
    Nothing -> do
      modify (first $ Map.insert ss (fromMaybe lbot $ Map.lookup ss mx))
      eval (memoEval eval) expr

drive :: forall m dom val expr env store.
         ( MonadEnv env m
         , MonadStore store m
         , Promote dom m
         , Ord expr
         , Ord env
         , Ord store
         , Ord val
         , Eq (dom val)
         , Lattice (dom val)) 
      => ((expr -> StateT (MemoTables dom val expr env store) m val) 
          -> (expr -> StateT (MemoTables dom val expr env store) m val))
      -> (expr -> m val)
drive eval expr = do
  let loop mx = do
        (v,(m1,mx')) <- runStateT (memoEval eval expr) (lbot,mx)
        if m1 == mx'
          then return v
          else loop m1
  loop lbot
