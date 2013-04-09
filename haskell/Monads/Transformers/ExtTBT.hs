{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.ExtTBT where

import Monads.Classes
import Util.ExtTB

instance (MonadEnvReader env m) => MonadEnvReader env (ExtTBT m) where
  askEnv = mAskEnv
  localEnv = mLocalEnv

instance (MonadEnvState env m) => MonadEnvState env (ExtTBT m) where
  getEnv = mGetEnv
  putEnv = mPutEnv

instance (MonadStoreState store m) => MonadStoreState store (ExtTBT m) where
  getStore = mGetStore
  putStore = mPutStore

instance (MonadTimeState time m) => MonadTimeState time (ExtTBT m) where
  getTime = mGetTime
  putTime = mPutTime
