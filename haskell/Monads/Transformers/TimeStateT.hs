{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.TimeStateT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader
import Util
import Monads.Transformers.StateT

newtype TimeStateT time m a = TimeStateT { unTimeStateT :: StateT time m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadFunctor
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env
  , MonadEnvState env
  , MonadStoreState store
  , MonadMorph n
  )

mkTimeStateT :: (time -> m (a,time)) -> TimeStateT time m a
mkTimeStateT = TimeStateT . StateT

runTimeStateT :: TimeStateT time m a -> time -> m (a,time)
runTimeStateT = runStateT . unTimeStateT

instance (MonadState s m) => MonadState s (TimeStateT time m) where
  get = monadGet
  put = monadPut

instance (Monad m) => MonadTimeState time (TimeStateT time m) where
  getTime = TimeStateT get
  putTime = TimeStateT . put
