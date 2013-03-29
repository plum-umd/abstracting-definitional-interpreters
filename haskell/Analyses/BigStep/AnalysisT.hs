{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, InstanceSigs #-}

module Analyses.BigStep.AnalysisT where

import Util
import Monads
import StateSpace
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Fixpoints.MemoEval as MemoEval
import qualified Data.Map as Map

newtype AnalysisT addr time dom var val m a = AnalysisT
  { unAnalysisT :: 
      EnvReaderT (Env var addr)
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
  , MonadEnvState env'
  , MonadStoreState (Store dom addr val)
  , MonadTimeState time
  , MonadMorph dom
  )

mkAnalysisT :: 
  (Monad m)
  => (Env var addr 
      -> Store dom addr val 
      -> time 
      -> m (a, Store dom addr val, time)
     ) 
  -> AnalysisT addr time dom var val m a
mkAnalysisT f = 
  AnalysisT $
  mkEnvReaderT $ \ env ->
  mkStoreStateT $ \ store ->
  mkTimeStateT $ \ time ->
  liftM unassociate $
  f env store time
  where
    unassociate (x,y,z) = ((x,y),z)

runAnalysisT :: 
  (Monad m) 
  => AnalysisT addr time dom var val m a
  -> Env var addr 
  -> Store dom addr val 
  -> time 
  -> m (a, Store dom addr val, time)
runAnalysisT aM env store time =
  liftM associate
  $ flip runTimeStateT time
  $ flip runStoreStateT store
  $ flip runEnvReaderT env
  $ unAnalysisT aM
  where
    associate ((x,y),z) = (x,y,z)

instance MonadTrans (AnalysisT addr time dom var val) where
  lift = AnalysisT . lift . lift . lift

instance MonadFunctor (AnalysisT addr time dom var val) where
  monadFmap f aMT = mkAnalysisT $ \env store time ->
    f $ runAnalysisT aMT env store time

