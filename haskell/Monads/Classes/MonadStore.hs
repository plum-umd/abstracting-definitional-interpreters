{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadStore where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List

class (Monad m) => MonadStore store m | m -> store where
  getStore :: m store
  putStore :: store -> m ()

modifyStore :: (MonadStore store m) => (store -> store) -> m ()
modifyStore f = do
  s <- getStore
  putStore (f s)

-- plumbing

instance (MonadStore store m) => MonadStore store (ListT m) where
  getStore = lift getStore
  putStore = lift . putStore

instance (MonadStore store m) => MonadStore store (ReaderT r m) where
  getStore = lift getStore
  putStore = lift . putStore

instance (MonadStore store m) => MonadStore store (StateT s m) where
  getStore = lift getStore
  putStore = lift . putStore
