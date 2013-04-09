{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.TimeStateT where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads.Classes
import Monads.Transformers.StateT
import Util.MFunctor

newtype TimeStateT time m a = TimeStateT { unTimeStateT :: StateT time m a}
  deriving 
  ( Monad
  , MonadTrans
  , MFunctor
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env
  , MonadEnvState env
  , MonadStoreState store
  , MMorph n
  )

mkTimeStateT :: (time -> m (a,time)) -> TimeStateT time m a
mkTimeStateT = TimeStateT . StateT

runTimeStateT :: TimeStateT time m a -> time -> m (a,time)
runTimeStateT = runStateT . unTimeStateT

instance (MonadState s m) => MonadState s (TimeStateT time m) where
  get = mGet
  put = mPut

instance (Monad m) => MonadTimeState time (TimeStateT time m) where
  getTime = TimeStateT get
  putTime = TimeStateT . put
