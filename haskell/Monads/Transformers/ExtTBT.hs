{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.ExtTBT where

import Monads.Classes
import Util

instance (MonadEnvReader env m) => MonadEnvReader env (ExtTBT m) where
  askEnv = monadAskEnv
  localEnv = monadLocalEnv

instance (MonadEnvState env m) => MonadEnvState env (ExtTBT m) where
  getEnv = monadGetEnv
  putEnv = monadPutEnv

instance (MonadStoreState store m) => MonadStoreState store (ExtTBT m) where
  getStore = monadGetStore
  putStore = monadPutStore

instance (MonadTimeState time m) => MonadTimeState time (ExtTBT m) where
  getTime = monadGetTime
  putTime = monadPutTime
