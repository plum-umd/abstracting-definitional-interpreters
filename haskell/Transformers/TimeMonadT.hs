{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Transformers.TimeMonadT where

import Classes
import Control.Monad.State
import Control.Monad.Reader

newtype TimeMonadT time m a = TimeMonadT { unTimeMonadT :: StateT time m a}
  deriving (Monad, MonadTrans, MonadState time)

instance (Monad m) => MonadTime time (TimeMonadT time m) where
  getTime = get
  putTime = put

-- mtl style lifting

instance (MonadEnv addr m) => MonadEnv addr (TimeMonadT time m) where
  askEnv = TimeMonadT $ lift $ askEnv
  localEnv f aM = TimeMonadT $ StateT $ \ s -> localEnv f (runStateT (unTimeMonadT aM) s)

instance (MonadStore dom addr val m) => MonadStore dom addr val (TimeMonadT time m) where
  getStore = TimeMonadT $ lift $ getStore
  putStore = TimeMonadT . lift . putStore

instance (Monad n, Promote m n) => Promote m (TimeMonadT time n) where
  promote = TimeMonadT . lift . promote
