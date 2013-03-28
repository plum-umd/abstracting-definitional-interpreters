{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.TimeStateT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader

newtype TimeStateT time m a = TimeStateT { unTimeStateT :: StateT time m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env
  , MonadEnvState env
  , MonadStoreState store
  )

runTimeStateT :: TimeStateT time m a -> time -> m (a,time)
runTimeStateT = runStateT . unTimeStateT

instance (MonadState s m) => MonadState s (TimeStateT time m) where
  get = lift get
  put = lift . put

instance (Monad m) => MonadTimeState time (TimeStateT time m) where
  getTime = TimeStateT get
  putTime = TimeStateT . put

instance (Monad n, Promote m n) => Promote m (TimeStateT time n) where
  promote = lift . promote
