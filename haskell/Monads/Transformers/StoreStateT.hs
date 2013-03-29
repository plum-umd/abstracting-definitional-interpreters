{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.StoreStateT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader
import Util
import Monads.Transformers.StateT

newtype StoreStateT store m a = StoreStateT { unStoreStateT :: StateT store m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadFunctor
  , MonadPlus
  , MonadReader r
  , MonadEnvReader env
  , MonadEnvState env
  , MonadTimeState time
  , MonadMorph n
  )

mkStoreStateT :: (store -> m (a,store)) -> StoreStateT store m a
mkStoreStateT = StoreStateT . StateT

runStoreStateT :: StoreStateT store m a -> store -> m (a,store)
runStoreStateT = runStateT . unStoreStateT

instance (MonadState s m) => MonadState s (StoreStateT store m) where
  get = monadGet
  put = monadPut

instance (Monad m) => MonadStoreState store (StoreStateT store m) where
  getStore = StoreStateT get
  putStore = StoreStateT . put
