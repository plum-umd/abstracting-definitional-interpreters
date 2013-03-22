{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.EnvMonadT where

import Control.Monad.Reader
import Control.Monad.State
import Monads.Classes

newtype EnvMonadT env m a = EnvMonadT { unEnvMonadT :: ReaderT env m a}
  deriving ( Monad
           , MonadTrans
           , MonadPlus
           , MonadState s
           , MonadStore store
           , MonadTime time
           )

runEnvMonadT :: EnvMonadT env m a -> env -> m a
runEnvMonadT = runReaderT . unEnvMonadT

mapEnvMonadT :: (m a -> n b) -> EnvMonadT env m a -> EnvMonadT env n b
mapEnvMonadT f = EnvMonadT . mapReaderT f . unEnvMonadT

instance (MonadReader r m) => MonadReader r (EnvMonadT env m) where
  ask = lift ask
  local = mapEnvMonadT . local

instance (Monad m) => MonadEnv env (EnvMonadT env m) where
  askEnv = EnvMonadT ask
  localEnv f = EnvMonadT . local f . unEnvMonadT

instance (Monad n, Promote m n) => Promote m (EnvMonadT addr n) where
  promote = lift . promote
