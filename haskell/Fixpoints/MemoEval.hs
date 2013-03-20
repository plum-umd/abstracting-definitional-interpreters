{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TupleSections, FlexibleContexts, ScopedTypeVariables #-}

module Fixpoints.MemoEval where

import Classes

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow
import Data.Maybe

type SS dom addr val expr = (expr,Env addr,Store dom addr val)
type MemoMap dom addr val expr = Map (SS dom addr val expr) (dom (val addr))
type MemoTables dom addr val expr = (MemoMap dom addr val expr, MemoMap dom addr val expr)

instance (MonadEnv addr m) => MonadEnv addr (StateT s m) where
  askEnv = StateT $ \ s -> liftM (,s) askEnv
  localEnv f aM = StateT $ \ s -> localEnv f (runStateT aM s)

instance (MonadStore dom addr val m) => MonadStore dom addr val (StateT s m) where
  getStore = StateT $ \s -> liftM (,s) getStore
  putStore store = StateT $ \s -> liftM (,s) $ putStore store

{- makes GHC explode 
instance (MonadTime time m) => MonadTime time (StateT s m) where
  getTime = StateT $ lift $ getTime
  putTime = StateT . lift . putTime
  -}
instance (MonadTime time m) => MonadTime time (StateT s m) where
  getTime = StateT $ \s -> liftM (,s) getTime
  putTime time = StateT $ \s -> liftM (,s) $ putTime time

instance (Monad n, Promote m n) => Promote m (StateT s n) where
  promote a = StateT $ \s -> liftM (,s) $ promote a

memoEval :: forall dom addr time val expr m.
            ( MonadEnv addr m, MonadStore dom addr val m, MonadTime time m
            , Addressable addr time
            , Promote dom m, Pointed dom
            , Ord addr
            , Ord expr
            , Ord (Env addr)
            , Ord (Store dom addr val)
            , Lattice (dom (val addr))
            ) 
         => ((expr -> StateT (MemoTables dom addr val expr) m (val addr))
             -> (expr -> StateT (MemoTables dom addr val expr) m (val addr)))
         -> (expr -> StateT (MemoTables dom addr val expr) m (val addr))
memoEval eval expr = do
  env <- askEnv
  store <- getStore
  let ss = (expr,env,store) 
  (m1,mx) <- get
  case Map.lookup ss m1 of
    Just vD -> promote vD
    Nothing -> do
      modify (first $ Map.insert ss (fromMaybe bot $ Map.lookup ss mx))
      eval (memoEval eval) expr

drive :: forall dom addr time val expr m.
         ( MonadEnv addr m, MonadStore dom addr val m, MonadTime time m
         , Addressable addr time
         , Promote dom m, Pointed dom
         , Ord addr
         , Ord expr
         , Ord (Env addr)
         , Ord (Store dom addr val)
         , Ord (dom (val addr))
         , Lattice (dom (val addr))
         ) 
      => ((expr -> StateT (MemoTables dom addr val expr) m (val addr)) 
          -> (expr -> StateT (MemoTables dom addr val expr) m (val addr)))
      -> (expr -> m (val addr))
drive eval expr = do
  let loop mx = do
        (v,(m1,mx')) <- runStateT (memoEval eval expr) (bot,mx)
        if m1 == mx'
          then return v
          else loop m1
  loop bot
