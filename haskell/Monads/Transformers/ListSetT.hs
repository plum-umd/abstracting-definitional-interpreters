{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.ListSetT where

import Monads.Classes
import Util

instance (MonadEnvReader env m) => MonadEnvReader env (ListSetT m) where
  askEnv = monadAskEnv
  localEnv = monadLocalEnv

instance (MonadEnvState env m) => MonadEnvState env (ListSetT m) where
  getEnv = monadGetEnv
  putEnv = monadPutEnv

instance (MonadStoreState store m) => MonadStoreState store (ListSetT m) where
  getStore = monadGetStore
  putStore = monadPutStore

instance (MonadTimeState time m) => MonadTimeState time (ListSetT m) where
  getTime = monadGetTime
  putTime = monadPutTime
