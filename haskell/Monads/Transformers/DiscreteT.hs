{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Monads.Transformers.DiscreteT where

import Monads.Transformers.ExtTBT
import Util.MFunctor
import Util.ExtTB
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Monads.Classes

newtype DiscreteT m a = DiscreteT { unDiscreteT :: ExtTBT m a}
  deriving 
  ( Monad
  , MonadTrans
  , MFunctor
  , MMonad
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

instance (Monad m) => MMorph ExtTB (DiscreteT m) where
  mMorph = DiscreteT . mMorph
