{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Transformers.EnvMonadT where

import Control.Monad.Reader
import Control.Monad.State
import Classes

newtype EnvMonadT addr m a = EnvMonadT { unEnvMonadT :: ReaderT (Env addr) m a}
  deriving (Monad, MonadTrans, MonadReader (Env addr))

instance (Monad m) => MonadEnv addr (EnvMonadT addr m) where
  askEnv = ask
  localEnv = local

-- mtl style lifting

instance (MonadStore dom addr val m) => MonadStore dom addr val (EnvMonadT addr m) where
  getStore = EnvMonadT $ lift $ getStore
  putStore = EnvMonadT . lift . putStore

instance (MonadTime time m) => MonadTime time (EnvMonadT addr m) where
  getTime = EnvMonadT $ lift $ getTime
  putTime = EnvMonadT . lift . putTime

instance (Monad n, Promote m n) => Promote m (EnvMonadT addr n) where
  promote = EnvMonadT . lift . promote
