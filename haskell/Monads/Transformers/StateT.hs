{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.StateT where

import Control.Monad.State
import Control.Monad.Trans
import Monads.Classes
import Util.MFunctor

instance MFunctor (StateT s) where
  mFmap f aMT =
    StateT $ f . runStateT aMT

instance (Monad m, MonadEnvReader env m) => MonadEnvReader env (StateT s m) where
  askEnv = mAskEnv
  localEnv = mLocalEnv

instance (Monad m, MonadEnvState store m) => MonadEnvState store (StateT s m) where
  getEnv = mGetEnv
  putEnv = mPutEnv

instance (Monad m, MonadStoreState store m) => MonadStoreState store (StateT s m) where
  getStore = mGetStore
  putStore = mPutStore

instance (Monad m, MonadTimeState time m) => MonadTimeState time (StateT s m) where
  getTime = mGetTime
  putTime = mPutTime

instance (Monad m, Monad n, MMorph m n) => MMorph m (StateT s n) where
  mMorph = mFmap mMorph . lift

