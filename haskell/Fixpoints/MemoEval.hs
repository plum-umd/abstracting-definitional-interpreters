{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TupleSections, FlexibleContexts, ScopedTypeVariables #-}

module Fixpoints.MemoEval where

import Debug.Trace
import Control.Exception
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
            ( MonadEnvReader env m
            , MonadStoreState store m
            , MonadState (MemoTables dom val expr env store) m
            , MonadMorph dom m
            , Ord expr
            , Ord env
            , Ord store
            , Ord val
            , Lattice (dom val)
            )
         => ((expr -> m (dom val)) -> (expr -> m (dom val)))
         -> (expr -> m (dom val))
memoEval eval expr = do
  env <- askEnv
  store <- getStore
  let ss = (expr,env,store) 
  (m1,mx) <- get :: m (MemoTables dom val expr env store)
  case Map.lookup ss m1 of
    Just vD -> return vD
    Nothing -> do
      modify (first $ ljoin (Map.singleton ss (fromMaybe lbot $ Map.lookup ss mx)))
      eval (memoEval eval) expr

{-
drive :: forall m dom val expr env store.
         ( MonadEnv env m
         , MonadStore store m
         , Promote dom m
         , Ord expr
         , Ord env
         , Ord store
         , Ord val
         , Eq (dom val)
         , Show (dom val)
         , Show store
         , Show env
         , Show expr
         , Lattice (dom val)) 
      => ((expr -> StateT (MemoTables dom val expr env store) m val) 
          -> (expr -> StateT (MemoTables dom val expr env store) m val))
      -> (expr -> m val)
drive eval expr = do
  let loop mx n = do
        (v,(m1,mx')) <- runStateT (memoEval eval expr) (lbot,mx)
        assert (mx == mx') $
          (if m1 /= mx' 
             then trace ("m1:  " ++ show m1 ++ "\n" ++ "mx': " ++ show mx' ++ "\n\n")
             else id) $
          if m1 == mx'
            then return v
            else if n == 0
              then return v
              else loop m1 (n-1)
  loop lbot 0
  -}

{-
  StateT store (ListT (StateT memo ID)) a = state -> memo -> ([a,S],memo)

  let loop mx = do
        let ([VxSxT],(m1,mx')) = runEverything (memoEval eval expr) (lbot,mx) S T
        assert (mx == mx') $
          if m1 == mx' 
            then [VxSxT]
            else loop m1



          (if m1 /= mx' 
             then trace ("m1:  " ++ show m1 ++ "\n" ++ "mx': " ++ show mx' ++ "\n\n")
             else id) $
          if m1 == mx'
            then return v
            else if n == 0
              then return v
              else loop m1 (n-1)
  loop lbot 0
-}
