{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.EnvReaderT where

import Control.Monad.Reader
import Control.Monad.State
import Monads.Classes

newtype EnvReaderT env m a = EnvReaderT { unEnvReaderT :: ReaderT env m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadPlus
  , MonadState s
  , MonadEnvState env'
  , MonadStoreState store
  , MonadTimeState time
  )

runEnvReaderT :: EnvReaderT env m a -> env -> m a
runEnvReaderT = runReaderT . unEnvReaderT

mapEnvReaderT :: (m a -> n b) -> EnvReaderT env m a -> EnvReaderT env n b
mapEnvReaderT f = EnvReaderT . mapReaderT f . unEnvReaderT

instance (MonadReader r m) => MonadReader r (EnvReaderT env m) where
  ask = lift ask
  local = mapEnvReaderT . local

instance (Monad m) => MonadEnvReader env (EnvReaderT env m) where
  askEnv = EnvReaderT ask
  localEnv f = EnvReaderT . local f . unEnvReaderT

instance (Monad n, Promote m n) => Promote m (EnvReaderT addr n) where
  promote = lift . promote
