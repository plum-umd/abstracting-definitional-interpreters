{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.EnvReaderT where

import Control.Monad.Reader
import Control.Monad.State
import Monads.Classes
import Monads.Transformers.ReaderT
import Util

newtype EnvReaderT env m a = EnvReaderT { unEnvReaderT :: ReaderT env m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadFunctor
  , MonadMonad
  , MonadPlus
  , MonadState s
  , MonadEnvState env'
  , MonadStoreState store
  , MonadTimeState time
  , MonadMorph n
  )

modifyEnvReaderT :: (ReaderT env m a -> ReaderT env m a) -> EnvReaderT env m a -> EnvReaderT env m a
modifyEnvReaderT f (EnvReaderT x) = EnvReaderT $ f x

mkEnvReaderT :: (env -> m a) -> EnvReaderT env m a
mkEnvReaderT = EnvReaderT . ReaderT

runEnvReaderT :: EnvReaderT env m a -> env -> m a
runEnvReaderT = runReaderT . unEnvReaderT

instance (MonadReader r m) => MonadReader r (EnvReaderT env m) where
  ask = monadAsk
  local = monadLocal

instance (Monad m) => MonadEnvReader env (EnvReaderT env m) where
  askEnv = EnvReaderT ask
  localEnv f = modifyEnvReaderT $ local f
