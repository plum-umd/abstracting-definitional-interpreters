{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.ListSetT where

import Monads.Classes
import Util.ListSet

instance (MonadEnvReader env m) => MonadEnvReader env (ListSetT m) where
  askEnv = mAskEnv
  localEnv = mLocalEnv

instance (MonadEnvState env m) => MonadEnvState env (ListSetT m) where
  getEnv = mGetEnv
  putEnv = mPutEnv

instance (MonadStoreState store m) => MonadStoreState store (ListSetT m) where
  getStore = mGetStore
  putStore = mPutStore

instance (MonadTimeState time m) => MonadTimeState time (ListSetT m) where
  getTime = mGetTime
  putTime = mPutTime
