{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.StoreStateT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader

newtype StoreStateT store m a = StoreStateT { unStoreStateT :: StateT store m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env
  , MonadEnvState env
  , MonadTimeState time
  )

runStoreStateT :: StoreStateT store m a -> store -> m (a,store)
runStoreStateT = runStateT . unStoreStateT

instance (MonadState s m) => MonadState s (StoreStateT store m) where
  get = lift get
  put = lift . put

instance (Monad m) => MonadStoreState store (StoreStateT store m) where
  getStore = StoreStateT get
  putStore = StoreStateT . put

instance (Monad n, Promote m n) => Promote m (StoreStateT store n) where
  promote = lift . promote
