{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances #-}

module Analyses.SmallStep.ConcreteT where

import Data.PartialOrder
import Data.Lattice
import StateSpace.Semantics
import Analyses.SmallStep.AnalysisT
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Fixpoints.Y
import Monads
import StateSpace
import Util.ExtTB
import Util.MFunctor
import qualified Data.Map as Map

newtype ConcreteT var val m a = ConcreteT
  { unConcreteT :: AnalysisT Integer Integer ExtTB var val (DiscreteT m) a }
  deriving
  ( Monad
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnvState (Env var Integer)
  , MonadStoreState (Store ExtTB Integer val)
  , MonadTimeState Integer
  , MMorph ExtTB
  )

mkConcreteT :: 
  (Monad m)
  => (Env var Integer
      -> Store ExtTB Integer val 
      -> Integer 
      -> m (ExtTB (a, Env var Integer, Store ExtTB Integer val, Integer))
     )
  -> ConcreteT var val m a
mkConcreteT f = 
  ConcreteT $
  mkAnalysisT $ \ env store time ->
  mkDiscreteT $
  f env store time

runConcreteT :: 
  (Monad m) 
  => ConcreteT var val m a
  -> Env var Integer 
  -> Store ExtTB Integer val 
  -> Integer 
  -> m (ExtTB (a, Env var Integer, Store ExtTB Integer val, Integer))
runConcreteT aM env store time =
  runDiscreteT 
  $ (\x -> runAnalysisT x env store time)
  $ unConcreteT aM

instance MonadTrans (ConcreteT var val) where
  lift = ConcreteT . lift . lift

instance MFunctor (ConcreteT var val) where
  mFmap f aMT = mkConcreteT $ \ env store time ->
    f $ runConcreteT aMT env store time

instance SmallStep (ConcreteT var val Identity) where
  newtype SS (ConcreteT var val Identity) a = ConcreteSS 
    { unConcreteSS :: ExtTB (a, Env var Integer, Store ExtTB Integer val, Integer) }
    deriving (PartialOrder)
  runSS step (ConcreteSS sss) = ConcreteSS $ do
    (e, env, store, time) <- sss
    runIdentity $ runConcreteT (step e) env store time

instance Inject (SS (ConcreteT var val Identity)) where
  inject a = ConcreteSS $ return (a, Map.empty, Map.empty, 0)
