{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}

module Analyses.BigStep.AbstractT where

import AAI.Addressable
import qualified Data.Map as Map
import Data.PartialOrder
import Data.Lattice
import Analyses.BigStep.AnalysisT
import StateSpace.Semantics
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
  , MonadStoreState (Store ListSet addr val)
  , MonadTimeState time
  , MMorph ListSet
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

instance MFunctor (AbstractT addr time var val) where
  mFmap f aMT = mkAbstractT $ \env store time ->
    f $ runAbstractT aMT env store time

instance BigStep (AbstractT addr time var val) where
  newtype InBS (AbstractT addr time var val) a = AbstractInBS
    { unAbstractInBS :: (a, Env var addr, Store ListSet addr val, time) }
    deriving (PartialOrder, Lattice, Eq, Ord)
  newtype OutBS (AbstractT addr time var val) a = AbstractOutBS
    { unAbstractOutBS :: ListSet (a, Store ListSet addr val, time) }
    deriving (PartialOrder, Lattice, Eq, Ord)
  askInBS e = do
    env <- askEnv
    store <- getStore
    time <- getTime
    return $ AbstractInBS (e, env, store, time)
  runBS eval (AbstractInBS (e, env, store, time)) = 
    liftM AbstractOutBS $ runAbstractT (eval e) env store time

instance (Addressable addr var time) => Inject (InBS (AbstractT addr time var val)) where
  inject a = AbstractInBS (a, Map.empty, Map.empty, tzero)
