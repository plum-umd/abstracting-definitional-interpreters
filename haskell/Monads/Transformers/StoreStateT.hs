{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.StoreStateT where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads.Classes
import Monads.Transformers.StateT
import Util.MFunctor

newtype StoreStateT store m a = StoreStateT { unStoreStateT :: StateT store m a}
  deriving 
  ( Monad
  , MonadTrans
  , MFunctor
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env
  , MonadEnvState env
  , MonadTimeState time
  , MMorph n
  )

mkStoreStateT :: (store -> m (a,store)) -> StoreStateT store m a
mkStoreStateT = StoreStateT . StateT

runStoreStateT :: StoreStateT store m a -> store -> m (a,store)
runStoreStateT = runStateT . unStoreStateT

instance (MonadState s m) => MonadState s (StoreStateT store m) where
  get = mGet
  put = mPut

instance (Monad m) => MonadStoreState store (StoreStateT store m) where
  getStore = StoreStateT get
  putStore = StoreStateT . put
