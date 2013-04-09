{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyses.SmallStep.AnalysisT where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads
import StateSpace
import Util.MFunctor

newtype AnalysisT addr time dom var val m a = AnalysisT
  { unAnalysisT :: 
      EnvStateT (Env var addr)
        (StoreStateT (Store dom addr val)
          (TimeStateT time 
            m)) 
        a 
  }
  deriving
  ( Monad
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnvReader (Env var addr)
  , MonadEnvState (Env var addr)
  , MonadStoreState (Store dom addr val)
  , MonadTimeState time
  , MMorph dom
  )

mkAnalysisT :: 
  (Monad m)
  => (Env var addr 
      -> Store dom addr val 
      -> time 
      -> m (a, Env var addr, Store dom addr val, time)
     ) 
  -> AnalysisT addr time dom var val m a
mkAnalysisT f = 
  AnalysisT $
  mkEnvStateT $ \ env ->
  mkStoreStateT $ \ store ->
  mkTimeStateT $ \ time ->
  liftM unassociate $
  f env store time
  where
    unassociate (w,x,y,z) = (((w,x),y),z)

runAnalysisT :: 
  (Monad m) 
  => AnalysisT addr time dom var val m a
  -> Env var addr 
  -> Store dom addr val 
  -> time 
  -> m (a, Env var addr, Store dom addr val, time)
runAnalysisT aM env store time =
  liftM associate
  $ flip runTimeStateT time
  $ flip runStoreStateT store
  $ flip runEnvStateT env
  $ unAnalysisT aM
  where
    associate (((w,x),y),z) = (w,x,y,z)

instance MonadTrans (AnalysisT addr time dom var val) where
  lift = AnalysisT . lift . lift . lift

instance MFunctor (AnalysisT addr time dom var val) where
  mFmap f aMT = mkAnalysisT $ \ env store time ->
    f $ runAnalysisT aMT env store time
