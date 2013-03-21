{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.StoreMonadT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader

newtype StoreMonadT store m a = StoreMonadT { unStoreMonadT :: StateT store m a}
  deriving ( Monad
           , MonadTrans
           , MonadPlus
           , MonadReader r
           , MonadEnv env
           , MonadTime time
           )

runStoreMonadT :: StoreMonadT store m a -> store -> m (a,store)
runStoreMonadT = runStateT . unStoreMonadT

instance (MonadState s m) => MonadState s (StoreMonadT store m) where
  get = lift get
  put = lift . put

instance (Monad m) => MonadStore store (StoreMonadT store m) where
  getStore = StoreMonadT get
  putStore = StoreMonadT . put

instance (Monad n, Promote m n) => Promote m (StoreMonadT store n) where
  promote = lift . promote
