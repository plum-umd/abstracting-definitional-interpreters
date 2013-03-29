{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.DiscreteT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List
import Util
import Monads.Transformers.ExtTBT

newtype DiscreteT m a = DiscreteT { unDiscreteT :: ExtTBT m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadFunctor
  , MonadMonad
  , MonadPlus
  , MonadReader r
  , MonadState s
  , MonadEnvReader env
  , MonadEnvState env
  , MonadStoreState store
  , MonadTimeState time
  )

mkDiscreteT :: m (ExtTB a) -> DiscreteT m a
mkDiscreteT = DiscreteT . ExtTBT

runDiscreteT :: DiscreteT m a -> m (ExtTB a)
runDiscreteT = runExtTBT . unDiscreteT

instance (Monad m) => MonadMorph ExtTB (DiscreteT m) where
  mmorph = DiscreteT . mmorph
