{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.EnvStateT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader
import Monads.Transformers.StateT
import Util

newtype EnvStateT env m a = EnvStateT { unEnvStateT :: StateT env m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadFunctor
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env'
  , MonadStoreState store
  , MonadTimeState time 
  , MonadMorph n
  )

runEnvStateT :: EnvStateT env m a -> env -> m (a,env)
runEnvStateT = runStateT . unEnvStateT

instance (MonadState s m) => MonadState s (EnvStateT env m) where
  get = monadGet
  put = monadPut

instance (Monad m) => MonadEnvState env (EnvStateT env m) where
  getEnv = EnvStateT get
  putEnv = EnvStateT . put
