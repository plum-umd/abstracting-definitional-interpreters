{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyses.SmallStep.AbstractT where

import Analyses.SmallStep.AnalysisT
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads
import StateSpace
import Util.ListSet
import Util.MFunctor

newtype AbstractT addr time var val m a = AbstractT
  { unAbstractT :: AnalysisT addr time ListSet var val (NonDetT m) a }
  deriving
  ( Monad
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnvReader (Env var addr)
  , MonadEnvState (Env var addr)
  , MonadStoreState (Store ListSet addr val)
  , MonadTimeState time
  , MMorph ListSet
  )

mkAbstractT :: 
  (Monad m)
  => (Env var addr 
      -> Store ListSet addr val 
      -> time 
      -> m (ListSet (a, Env var addr, Store ListSet addr val, time))
     ) 
  -> AbstractT addr time var val m a
mkAbstractT f = 
  AbstractT $
  mkAnalysisT $ \ env store time ->
  mkNonDetT $
  f env store time

runAbstractT :: 
  (Monad m) 
  => AbstractT addr time var val m a
  -> Env var addr 
  -> Store ListSet addr val 
  -> time 
  -> m (ListSet (a, Env var addr, Store ListSet addr val, time))
runAbstractT aM env store time =
  runNonDetT 
  $ (\x -> runAnalysisT x env store time)
  $ unAbstractT aM

instance MonadTrans (AbstractT addr time var val) where
  lift = AbstractT . lift . lift

instance MFunctor (AbstractT addr time var val) where
  mFmap f aMT = mkAbstractT $ \env store time ->
    f $ runAbstractT aMT env store time

