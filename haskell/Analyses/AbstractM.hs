{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, InstanceSigs #-}

module Analyses.AbstractM where

import Util
import Monads
import StateSpace
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Fixpoints.MemoEval as MemoEval
import qualified Data.Map as Map

newtype AbstractMT addr time var val m a = AbstractMT
  { unAbstractMT :: EnvMonadT (Env var addr)
                   (StoreMonadT (Store ListSet addr val)
                   (TimeMonadT time 
                   (NonDetT 
                   m))) a }
  deriving
  ( Monad
  -- , MonadTrans
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnv (Env var addr)
  , MonadStore (Store ListSet addr val)
  , MonadTime time
  , Promote ListSet
  )

instance MonadTrans (AbstractMT addr time var val) where
  lift = AbstractMT . lift . lift . lift . lift

runAbstractMT :: (Monad m) 
              => AbstractMT addr time var val m a
              -> Env var addr 
              -> Store ListSet addr val 
              -> time 
              -> m (ListSet (a, Store ListSet addr val, time))
runAbstractMT aM env store time =
  let rs =
        runNonDetT 
        $ flip runTimeMonadT time
        $ flip runStoreMonadT store
        $ flip runEnvMonadT env
        $ unAbstractMT aM
  in liftM (liftM (\ ((a,store),time) -> (a,store,time))) rs

