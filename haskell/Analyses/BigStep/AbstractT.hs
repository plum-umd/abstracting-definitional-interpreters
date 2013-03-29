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
import Analyses.BigStep.AnalysisT

newtype AbstractT addr time var val m a = AbstractT
  { unAbstractT :: AnalysisT addr time ListSet var val (NonDetT m) a }
  deriving
  ( Monad
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnvReader (Env var addr)
  , MonadEnvState env'
  , MonadStoreState (Store ListSet addr val)
  , MonadTimeState time
  , MonadMorph ListSet
  )

mkAbstractT :: 
  (Monad m)
  => (Env var addr 
      -> Store ListSet addr val 
      -> time 
      -> m (ListSet (a, Store ListSet addr val, time))
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
  -> m (ListSet (a, Store ListSet addr val, time))
runAbstractT aM env store time =
  runNonDetT 
  $ (\x -> runAnalysisT x env store time)
  $ unAbstractT aM

instance MonadTrans (AbstractT addr time var val) where
  lift = AbstractT . lift . lift

instance MonadFunctor (AbstractT addr time var val) where
  monadFmap f aMT = mkAbstractT $ \env store time ->
    f $ runAbstractT aMT env store time
