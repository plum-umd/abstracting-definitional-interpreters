{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.EnvReaderT where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads.Classes
import Monads.Transformers.ReaderT
import Util.MFunctor

newtype EnvReaderT env m a = EnvReaderT { unEnvReaderT :: ReaderT env m a}
  deriving 
  ( Monad
  , MonadTrans
  , MFunctor
  , MMonad
  , MonadPlus
  , MonadState s
  , MonadEnvState env'
  , MonadStoreState store
  , MonadTimeState time
  , MMorph n
  )

modifyEnvReaderT :: (ReaderT env m a -> ReaderT env m a) -> EnvReaderT env m a -> EnvReaderT env m a
modifyEnvReaderT f (EnvReaderT x) = EnvReaderT $ f x

mkEnvReaderT :: (env -> m a) -> EnvReaderT env m a
mkEnvReaderT = EnvReaderT . ReaderT

runEnvReaderT :: EnvReaderT env m a -> env -> m a
runEnvReaderT = runReaderT . unEnvReaderT

instance (MonadReader r m) => MonadReader r (EnvReaderT env m) where
  ask = mAsk
  local = mLocal

instance (Monad m) => MonadEnvReader env (EnvReaderT env m) where
  askEnv = EnvReaderT ask
  localEnv f = modifyEnvReaderT $ local f
