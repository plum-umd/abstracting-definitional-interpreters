{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyses.BigStep.ConcreteT where

import qualified Data.Map as Map
import Fixpoints
import Control.Monad.Identity
import Monads
import StateSpace
import Util
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Analyses.BigStep.AnalysisT

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
  , MonadMorph ExtTB
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

instance MonadFunctor (ConcreteT var val) where
  monadFmap f aMT = mkConcreteT $ \ env store time ->
    f $ runConcreteT aMT env store time

type Concrete_Driver var val a = ConcreteT var val Identity a

driveConcrete ::
  ((expr -> Concrete_Driver var val (ExtTB val))
   -> (expr -> Concrete_Driver var val (ExtTB val))
  )
  -> expr 
  -> ExtTB (ExtTB val, Store ExtTB Integer val, Integer)
driveConcrete eval e = runIdentity $ runConcreteT (yEval eval e) Map.empty Map.empty 0
