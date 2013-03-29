{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.ReaderT where

import Util
import Control.Monad.Reader
import Monads.Classes

instance MonadFunctor (ReaderT r) where
  monadFmap = monadMapM

instance MonadMonad (ReaderT r) where
  monadExtend f aMT =
    ReaderT $ \r ->
    runReaderT (f $ runReaderT aMT r) r

instance (Monad m, MonadEnvReader env m) => MonadEnvReader env (ReaderT r m) where
  askEnv = monadAskEnv
  localEnv = monadLocalEnv

instance (Monad m, MonadEnvState store m) => MonadEnvState store (ReaderT r m) where
  getEnv = monadGetEnv
  putEnv = monadPutEnv

instance (Monad m, MonadStoreState store m) => MonadStoreState store (ReaderT r m) where
  getStore = monadGetStore
  putStore = monadPutStore

instance (Monad m, MonadTimeState time m) => MonadTimeState time (ReaderT r m) where
  getTime = monadGetTime
  putTime = monadPutTime

instance (Monad m, Monad n, MonadMorph m n) => MonadMorph m (ReaderT r n) where
  mmorph = monadFmap mmorph . lift
