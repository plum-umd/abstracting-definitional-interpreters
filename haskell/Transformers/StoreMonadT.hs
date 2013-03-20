{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Transformers.StoreMonadT where

import Classes
import Control.Monad.State
import Control.Monad.Reader

newtype StoreMonadT dom addr val m a = StoreMonadT { unStoreMonadT :: StateT (Store dom addr val) m a}
  deriving (Monad, MonadTrans, MonadState (Store dom addr val))

instance (Monad m) => MonadStore dom addr val (StoreMonadT dom addr val m) where
  getStore = get
  putStore = put

-- mtl style lifting

instance (MonadEnv addr m) => MonadEnv addr (StoreMonadT dom addr val m) where
  askEnv = StoreMonadT $ lift $ askEnv
  localEnv f aM = StoreMonadT $ StateT $ \ s -> localEnv f (runStateT (unStoreMonadT aM) s)

instance (MonadTime time m) => MonadTime time (StoreMonadT dom addr val m) where
  getTime = StoreMonadT $ lift $ getTime
  putTime = StoreMonadT . lift . putTime

instance (Monad n, Promote m n) => Promote m (StoreMonadT dom addr val n) where
  promote = StoreMonadT . lift . promote
