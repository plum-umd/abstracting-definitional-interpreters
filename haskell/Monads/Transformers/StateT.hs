{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.StateT where

import Util
import Control.Monad.State
import Monads.Classes

instance MonadFunctor (StateT s) where
  monadFmap f aMT =
    StateT $ f . runStateT aMT

instance (Monad m, MonadEnvReader env m) => MonadEnvReader env (StateT s m) where
  askEnv = monadAskEnv
  localEnv = monadLocalEnv

instance (Monad m, MonadEnvState store m) => MonadEnvState store (StateT s m) where
  getEnv = monadGetEnv
  putEnv = monadPutEnv

instance (Monad m, MonadStoreState store m) => MonadStoreState store (StateT s m) where
  getStore = monadGetStore
  putStore = monadPutStore

instance (Monad m, MonadTimeState time m) => MonadTimeState time (StateT s m) where
  getTime = monadGetTime
  putTime = monadPutTime

instance (Monad m, Monad n, MonadMorph m n) => MonadMorph m (StateT s n) where
  mmorph = monadFmap mmorph . lift

