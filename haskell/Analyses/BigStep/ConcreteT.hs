{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyses.BigStep.ConcreteT where

import Analyses.BigStep.AnalysisT
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Fixpoints.YEval
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
  , MonadEnvReader (Env var Integer)
  , MonadEnvState env'
  , MonadStoreState (Store ExtTB Integer val)
  , MonadTimeState Integer
  , MMorph ExtTB
  )

mkConcreteT :: 
  (Monad m)
  => (Env var Integer
      -> Store ExtTB Integer val 
      -> Integer 
      -> m (ExtTB (a, Store ExtTB Integer val, Integer))
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
  -> m (ExtTB (a, Store ExtTB Integer val, Integer))
runConcreteT aM env store time =
  runDiscreteT 
  $ (\x -> runAnalysisT x env store time)
  $ unConcreteT aM

instance MonadTrans (ConcreteT var val) where
  lift = ConcreteT . lift . lift

instance MFunctor (ConcreteT var val) where
  mFmap f aMT = mkConcreteT $ \ env store time ->
    f $ runConcreteT aMT env store time

type ConcreteDriver var val a = ConcreteT var val Identity a

driveConcrete ::
  ((expr -> ConcreteDriver var val (ExtTB val))
   -> expr 
   -> ConcreteDriver var val (ExtTB val)
  )
  -> expr 
  -> ExtTB (ExtTB val, Store ExtTB Integer val, Integer)
driveConcrete eval e = runIdentity $ runConcreteT (yEval eval e) Map.empty Map.empty 0
