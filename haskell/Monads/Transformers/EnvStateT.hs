{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.EnvStateT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader

newtype EnvStateT env m a = EnvStateT { unEnvStateT :: StateT env m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env'
  , MonadStoreState store
  , MonadTimeState time 
  )

runEnvStateT :: EnvStateT env m a -> env -> m (a,env)
runEnvStateT = runStateT . unEnvStateT

instance (MonadState s m) => MonadState s (EnvStateT env m) where
  get = lift get
  put = lift . put

instance (Monad m) => MonadEnvState env (EnvStateT env m) where
  getEnv = EnvStateT get
  putEnv = EnvStateT . put

instance (Monad n, Promote m n) => Promote m (EnvStateT env n) where
  promote = lift . promote

