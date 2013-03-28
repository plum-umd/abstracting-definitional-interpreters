{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, InstanceSigs #-}

module Analyses.BigStep.AbstractT where

import Util
import Monads
import StateSpace
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Fixpoints.MemoEval as MemoEval
import qualified Data.Map as Map

newtype AbstractT addr time var val m a = AbstractT
  { unAbstractT :: 
      EnvReaderT 
        (Env var addr)
        (StoreStateT (Store ListSet addr val)
          (TimeStateT time 
            (NonDetT 
              m))) 
        a 
  }
  deriving
  ( Monad
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnvReader (Env var addr)
  , MonadEnvState env'
  , MonadStoreState (Store ListSet addr val)
  , MonadTimeState time
  , Promote ListSet
  )

instance MonadTrans (AbstractT addr time var val) where
  lift = AbstractT . lift . lift . lift . lift

runAbstractT :: 
  (Monad m) 
  => AbstractT addr time var val m a
  -> Env var addr 
  -> Store ListSet addr val 
  -> time 
  -> m (ListSet (a, Store ListSet addr val, time))
runAbstractT aM env store time =
  liftM (liftM associate) 
  $ runNonDetT 
  $ flip runTimeStateT time
  $ flip runStoreStateT store
  $ flip runEnvReaderT env
  $ unAbstractT aM
  where
    associate ((x,y),z) = (x,y,z)
