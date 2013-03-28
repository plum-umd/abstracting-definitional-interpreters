{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadStoreState where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List
import Util.ListSet

class (Monad m) => MonadStoreState store m | m -> store where
  getStore :: m store
  putStore :: store -> m ()

modifyStore :: (MonadStoreState store m) => (store -> store) -> m ()
modifyStore f = do
  s <- getStore
  putStore $ f s

-- plumbing

instance (MonadStoreState store m) => MonadStoreState store (ListSetT m) where
  getStore = lift getStore
  putStore = lift . putStore

instance (MonadStoreState store m) => MonadStoreState store (ReaderT r m) where
  getStore = lift getStore
  putStore = lift . putStore

instance (MonadStoreState store m) => MonadStoreState store (StateT s m) where
  getStore = lift getStore
  putStore = lift . putStore
